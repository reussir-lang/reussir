//! End-to-end examples: build Reussir IR with the high-level wrappers
//! ([`reussir_backend::dialect`] op builders + [`reussir_backend::dialect::ty`]
//! type constructors), lower it with the Reussir pipeline, and execute it
//! through the [`OrcJit`].
//!
//! Each test constructs a small `func.func` out of Reussir operations, lowers it
//! to the LLVM dialect, adds it to a persistent ORC session built with
//! [`OrcJit::with_runtime`] (which defines the linked-in runtime's `__reussir_*`
//! symbols in-process, so allocator calls resolve without locating a shared
//! library), looks the function up and calls it directly through the C ABI.

use reussir_backend::dialect::{self, ty};
use reussir_backend::melior::Context;
use reussir_backend::melior::dialect::{arith, func};
use reussir_backend::melior::ir::attribute::{
    DenseI32ArrayAttribute, IntegerAttribute, StringAttribute, TypeAttribute,
};
use reussir_backend::melior::ir::block::BlockLike;
use reussir_backend::melior::ir::operation::{OperationBuilder, OperationLike};
use reussir_backend::melior::ir::r#type::FunctionType;
use reussir_backend::melior::ir::{
    Block, Identifier, Location, Module, Operation, Region, RegionLike, Type, Value,
};
use reussir_backend::pipeline::{LoweringOptions, run_lowering_pipeline};
use reussir_jit::{OptLevel, OrcJit};

// Adds a nullary `func.func @name() -> result_type` whose body is produced by
// `body` (which must terminate the block, e.g. with `func.return`) to `module`.
fn add_function<'c>(
    context: &'c Context,
    module: &Module<'c>,
    name: &str,
    result_type: Type<'c>,
    body: impl FnOnce(&Block<'c>),
) {
    let location = Location::unknown(context);
    let block = Block::new(&[]);
    body(&block);

    let region = Region::new();
    region.append_block(block);

    let function = func::func(
        context,
        StringAttribute::new(context, name),
        TypeAttribute::new(FunctionType::new(context, &[], &[result_type]).into()),
        region,
        &[],
        location,
    );
    module.body().append_operation(function);
}

// Appends `operation` and returns its first result as a value. Accepts both
// melior's plain `Operation` builders and the generated typed Reussir op structs
// (which convert via `Into<Operation>`).
fn result_of<'c, 'a>(block: &'a Block<'c>, operation: impl Into<Operation<'c>>) -> Value<'c, 'a> {
    block
        .append_operation(operation.into())
        .result(0)
        .unwrap()
        .into()
}

// Builds a `reussir.rc.create` that takes only a value (no token/region). The op
// carries `AttrSizedOperandSegments`, and melior's generated builder does not
// populate `operandSegmentSizes`, so we construct it directly and set the segment
// sizes ([value, token, region] = [1, 0, 0]). The pipeline's token-instantiation
// pass supplies the missing allocation token.
fn rc_create_value<'c, 'a>(
    context: &'c Context,
    block: &'a Block<'c>,
    rc_type: Type<'c>,
    value: Value<'c, '_>,
    location: Location<'c>,
) -> Value<'c, 'a> {
    let operation = OperationBuilder::new("reussir.rc.create", location)
        .add_operands(&[value])
        .add_attributes(&[(
            Identifier::new(context, "operandSegmentSizes"),
            DenseI32ArrayAttribute::new(context, &[1, 0, 0]).into(),
        )])
        .add_results(&[rc_type])
        .build()
        .expect("valid reussir.rc.create");
    block.append_operation(operation).result(0).unwrap().into()
}

// Lowers `module` to the LLVM dialect and adds it to a fresh ORC session whose
// Reussir runtime symbols are resolved in-process (the runtime is linked into
// this test binary through the `reussir-jit` crate).
fn jit_module(context: &Context, module: &mut Module) -> OrcJit {
    run_lowering_pipeline(context, module, &LoweringOptions::default())
        .expect("lowering pipeline should succeed");
    assert!(
        module.as_operation().verify(),
        "lowered module should verify"
    );

    let jit = OrcJit::with_runtime().expect("create JIT with the runtime");
    jit.add_module(module, OptLevel::Default)
        .expect("add module to JIT");
    jit
}

// Looks up a nullary function returning a 64-bit-sized scalar and calls it.
fn call_returning_i64(jit: &OrcJit, name: &str) -> i64 {
    let address = jit.lookup(name).expect("symbol should be found");
    let function: extern "C" fn() -> i64 = unsafe { std::mem::transmute(address as usize) };
    function()
}

/// Example 1: allocate and free a memory token through the runtime allocator.
/// `reussir.token.alloc` / `reussir.token.free` lower to `__reussir_allocate` /
/// `__reussir_deallocate`; the function returns 0 to prove it ran.
#[test]
fn token_alloc_free_round_trip() {
    let context = reussir_backend::context();
    let i64_type = Type::parse(&context, "i64").unwrap();
    let mut module = Module::new(Location::unknown(&context));

    add_function(&context, &module, "tok", i64_type, |block| {
        let location = Location::unknown(&context);
        let token_type = ty::token(&context, 8, 16);

        let token = result_of(block, dialect::token_alloc(&context, token_type, location));
        block.append_operation(dialect::token_free(&context, token, location).into());

        let zero = result_of(
            block,
            arith::constant(
                &context,
                IntegerAttribute::new(i64_type, 0).into(),
                location,
            ),
        );
        block.append_operation(func::r#return(&[zero], location));
    });

    let jit = jit_module(&context, &mut module);
    assert_eq!(call_returning_i64(&jit, "tok"), 0);
}

/// Example 2: a manual heap round-trip with no reference counting. Allocate a
/// token, reinterpret it as a typed reference, store 42 through it, load the value
/// back, then free the token. Exercises `reussir.token.alloc`,
/// `reussir.token.reinterpret`, `memref.store`, `reussir.ref.load` and
/// `reussir.token.free`.
#[test]
fn token_reinterpret_store_load() {
    let context = reussir_backend::context();
    let i64_type = Type::parse(&context, "i64").unwrap();
    let mut module = Module::new(Location::unknown(&context));

    add_function(&context, &module, "store_load", i64_type, |block| {
        let location = Location::unknown(&context);
        // Token alignment must match the reinterpreted element's alignment.
        let token_type = ty::token(&context, 4, 8);
        // Raw allocation is unrelated to regional field capability.
        let ref_type = ty::r#ref(
            i64_type,
            ty::ReussirCapability::Unspecified,
            ty::ReussirAtomicKind::Normal,
        );

        let token = result_of(block, dialect::token_alloc(&context, token_type, location));
        let reference = result_of(
            block,
            dialect::token_reinterpret(&context, ref_type, token, location),
        );
        let value = result_of(
            block,
            arith::constant(
                &context,
                IntegerAttribute::new(i64_type, 42).into(),
                location,
            ),
        );
        let view_type = Type::parse(&context, "memref<i64>").unwrap();
        let view = result_of(
            block,
            OperationBuilder::new("reussir.ref.to_memref", location)
                .add_operands(&[reference])
                .add_results(&[view_type])
                .build()
                .unwrap(),
        );
        block.append_operation(
            OperationBuilder::new("memref.store", location)
                .add_operands(&[value, view])
                .build()
                .unwrap(),
        );
        let loaded = result_of(
            block,
            dialect::ref_load(&context, i64_type, reference, location),
        );
        block.append_operation(dialect::token_free(&context, token, location).into());
        block.append_operation(func::r#return(&[loaded], location));
    });

    let jit = jit_module(&context, &mut module);
    assert_eq!(call_returning_i64(&jit, "store_load"), 42);
}

/// Example 3: create an `!reussir.rc<i64>` on the heap and read its reference
/// count with `reussir.rc.fetch`. A freshly created Rc has a count of 1.
#[test]
fn rc_create_fetch_refcount() {
    let context = reussir_backend::context();
    let i64_type = Type::parse(&context, "i64").unwrap();
    let index_type = Type::parse(&context, "index").unwrap();
    let mut module = Module::new(Location::unknown(&context));

    add_function(&context, &module, "rc_refcount", index_type, |block| {
        let location = Location::unknown(&context);
        let rc_type = ty::rc(
            i64_type,
            ty::ReussirCapability::Shared,
            ty::ReussirAtomicKind::Normal,
        );

        let value = result_of(
            block,
            arith::constant(
                &context,
                IntegerAttribute::new(i64_type, 0).into(),
                location,
            ),
        );
        let rc = rc_create_value(&context, block, rc_type, value, location);
        let count = result_of(block, dialect::rc_fetch(&context, index_type, rc, location));
        block.append_operation(func::r#return(&[count], location));
    });

    let jit = jit_module(&context, &mut module);
    // `index` is pointer-sized; on a 64-bit host it reads back as an i64.
    assert_eq!(call_returning_i64(&jit, "rc_refcount"), 1);
}

/// Example 4: arbitrary-precision integers. 25! outgrows the immediate range,
/// so the multiplications and the comparison call the linked-in rcgmp runtime,
/// whose entry points [`OrcJit::with_runtime`] defines alongside the Reussir
/// runtime's. Both JIT code generators run it, TPDE being the REPL's default.
#[test]
fn rcgmp_factorial_calls_the_linked_in_runtime() {
    if !reussir_backend::features::enabled("rcgmp") {
        return;
    }
    let mut levels = vec![OptLevel::Default];
    if reussir_backend::features::enabled("tpde") {
        levels.push(OptLevel::Tpde);
    }
    for level in levels {
        assert_eq!(rcgmp_factorial_matches(level), 1, "{level:?}");
    }
}

fn rcgmp_factorial_matches(level: OptLevel) -> i64 {
    let context = reussir_backend::context();
    let source = r#"
        module {
          func.func @factorial_matches() -> i64 {
            %one = rcgmp.constant 1 : !rcgmp.mpz
            %c1 = arith.constant 1 : i64
            %c26 = arith.constant 26 : i64
            %f = scf.for %i = %c1 to %c26 step %c1 iter_args(%acc = %one) -> (!rcgmp.mpz) : i64 {
              %k = rcgmp.extsi %i : i64 to !rcgmp.mpz
              %next = rcgmp.muli %acc, %k : !rcgmp.mpz
              scf.yield %next : !rcgmp.mpz
            }
            %expected = rcgmp.constant 15511210043330985984000000 : !rcgmp.mpz
            %eq = rcgmp.cmpi eq, %f, %expected : !rcgmp.mpz
            rcgmp.release %f : !rcgmp.mpz
            %r = arith.extui %eq : i1 to i64
            func.return %r : i64
          }
        }
    "#;
    let mut module = Module::parse(&context, source).expect("module should parse");
    let options = LoweringOptions {
        opt: level,
        ..LoweringOptions::default()
    };
    run_lowering_pipeline(&context, &mut module, &options)
        .expect("lowering pipeline should succeed");

    let jit = OrcJit::with_runtime().expect("create JIT with the runtimes");
    jit.add_module(&module, level).expect("add module to JIT");
    call_returning_i64(&jit, "factorial_matches")
}
