//! Hand-written op builders for cases melior's generated builders don't cover
//! well enough.
//!
//! melior's ODS/dialect builders are preferred everywhere they fit; this module
//! holds the few operations whose generated builder is missing a needed
//! parameter (so the result/attributes can't be set), keeping the raw
//! [`OperationBuilder`] usage contained in the backend rather than leaking into
//! downstream code generators.

use melior::Context;
use melior::ir::attribute::{
    ArrayAttribute, Attribute, DenseI32ArrayAttribute, DenseI64ArrayAttribute,
    FlatSymbolRefAttribute, IntegerAttribute, StringAttribute, TypeAttribute,
};
use melior::ir::operation::{OperationBuilder, OperationLike, OperationRef};
use melior::ir::r#type::IntegerType;
use melior::ir::{Identifier, Location, Operation, Region, Type, Value, ValueLike};

use reussir_backend_sys as sys;

/// Set `operation`'s location after it has been built.
///
/// Used to attach a fused debug-info location (carrying a `DBGLocalVar`) to the
/// op that defines a local variable; `mlir-sys` does not bind
/// `mlirOperationSetLocation`, so this goes through the Reussir C API.
pub fn set_location(operation: OperationRef<'_, '_>, location: Location<'_>) {
    unsafe {
        sys::reussirOperationSetLocation(operation.to_raw(), location.to_raw());
    }
}

/// `arith.truncf %value : <from> to <result_type>`.
///
/// melior's [`arith::truncf`](melior::dialect::arith::truncf) is a plain unary
/// with no result-type parameter, so it cannot express a float *narrowing* (the
/// result width must differ from the operand's). This sets the result type
/// explicitly.
pub fn truncf<'c>(
    value: Value<'c, '_>,
    result_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("arith.truncf", location)
        .add_operands(&[value])
        .add_results(&[result_type])
        .build()
        .expect("valid arith.truncf")
}

/// `reussir.trampoline export "<abi>" @<sym_name> = @<target>`.
///
/// The op is attribute-only and its `direction` is a `TrampolineDirection`
/// `I32EnumAttr` for which neither melior nor the Reussir C API exposes a
/// constructor; the enum is stored as a signless `i32` (`Import` is case `0`,
/// `Export` case `1`). Building it here keeps that representational detail
/// out of the code generator.
pub fn trampoline_export<'c>(
    context: &'c Context,
    abi_name: &str,
    sym_name: &str,
    target: &str,
    location: Location<'c>,
) -> Operation<'c> {
    trampoline(context, 1, abi_name, sym_name, target, location)
}

/// `reussir.trampoline import "<abi>" @<sym_name> = @<target>`: `target` is a
/// body-less declaration carrying the native signature (whose definition the
/// lowering materializes) and `sym_name` the external boundary symbol.
pub fn trampoline_import<'c>(
    context: &'c Context,
    abi_name: &str,
    sym_name: &str,
    target: &str,
    location: Location<'c>,
) -> Operation<'c> {
    trampoline(context, 0, abi_name, sym_name, target, location)
}

fn trampoline<'c>(
    context: &'c Context,
    direction: i64,
    abi_name: &str,
    sym_name: &str,
    target: &str,
    location: Location<'c>,
) -> Operation<'c> {
    let direction = IntegerAttribute::new(IntegerType::new(context, 32).into(), direction).into();
    OperationBuilder::new("reussir.trampoline", location)
        .add_attributes(&[
            (Identifier::new(context, "direction"), direction),
            (
                Identifier::new(context, "target"),
                FlatSymbolRefAttribute::new(context, target).into(),
            ),
            (
                Identifier::new(context, "abi_name"),
                StringAttribute::new(context, abi_name).into(),
            ),
            (
                Identifier::new(context, "sym_name"),
                StringAttribute::new(context, sym_name).into(),
            ),
        ])
        .build()
        .expect("valid reussir.trampoline")
}

/// `reussir.polyffi texture("...")` — an uncompiled foreign source texture;
/// the `reussir-compile-polymorphic-ffi` pass compiles it to bitcode that the
/// final translation links into the module.
pub fn polyffi_texture<'c>(
    context: &'c Context,
    texture: &str,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.polyffi", location)
        .add_attributes(&[(
            Identifier::new(context, "moduleTexture"),
            StringAttribute::new(context, texture).into(),
        )])
        .build()
        .expect("valid reussir.polyffi")
}

/// `reussir.str.global @<sym_name> = "payload"`.
pub fn str_global<'c>(
    context: &'c Context,
    sym_name: &str,
    payload: &str,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.str.global", location)
        .add_attributes(&[
            (
                Identifier::new(context, "sym_name"),
                StringAttribute::new(context, sym_name).into(),
            ),
            (
                Identifier::new(context, "payload"),
                StringAttribute::new(context, payload).into(),
            ),
        ])
        .build()
        .expect("valid reussir.str.global")
}

/// `reussir.str.literal @<sym_name> : <result_type>`.
pub fn str_literal<'c>(
    context: &'c Context,
    sym_name: &str,
    result_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.str.literal", location)
        .add_attributes(&[(
            Identifier::new(context, "sym_name"),
            FlatSymbolRefAttribute::new(context, sym_name).into(),
        )])
        .add_results(&[result_type])
        .build()
        .expect("valid reussir.str.literal")
}

/// `reussir.const_seq.global @<sym_name> (: <seq_type>)? = <payload>`, where
/// `payload` is a rank-1 dense elements attribute (`seq_type` then omitted)
/// or, for a sequence of aggregates, an array of the elements' constants
/// with `seq_type` the `!reussir.const_seq<…>` type.
pub fn const_seq_global<'c>(
    context: &'c Context,
    sym_name: &str,
    seq_type: Option<Type<'c>>,
    payload: Attribute<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    let mut attributes = vec![
        (
            Identifier::new(context, "sym_name"),
            StringAttribute::new(context, sym_name).into(),
        ),
        (Identifier::new(context, "payload"), payload),
    ];
    if let Some(seq_type) = seq_type {
        attributes.push((
            Identifier::new(context, "seq_type"),
            TypeAttribute::new(seq_type).into(),
        ));
    }
    OperationBuilder::new("reussir.const_seq.global", location)
        .add_attributes(&attributes)
        .build()
        .expect("valid reussir.const_seq.global")
}

/// `reussir.const_seq.literal @<sym_name> : <result_type>`.
pub fn const_seq_literal<'c>(
    context: &'c Context,
    sym_name: &str,
    result_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.const_seq.literal", location)
        .add_attributes(&[(
            Identifier::new(context, "sym_name"),
            FlatSymbolRefAttribute::new(context, sym_name).into(),
        )])
        .add_results(&[result_type])
        .build()
        .expect("valid reussir.const_seq.literal")
}

/// `reussir.const_seq.len (<seq>) : index`.
pub fn const_seq_len<'c>(
    context: &'c Context,
    seq: Value<'c, '_>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.const_seq.len", location)
        .add_operands(&[seq])
        .add_results(&[Type::index(context)])
        .build()
        .expect("valid reussir.const_seq.len")
}

/// `reussir.const_seq.get (<seq>) [<index>] : <element_type>` — panics when
/// the index is out of bounds.
pub fn const_seq_get<'c>(
    seq: Value<'c, '_>,
    index: Value<'c, '_>,
    element_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.const_seq.get", location)
        .add_operands(&[seq, index])
        .add_results(&[element_type])
        .build()
        .expect("valid reussir.const_seq.get")
}

/// `reussir.str.cast (<global>) : <result_type>`.
pub fn str_cast<'c>(
    value: Value<'c, '_>,
    result_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.str.cast", location)
        .add_operands(&[value])
        .add_results(&[result_type])
        .build()
        .expect("valid reussir.str.cast")
}

/// `reussir.str.select (<str>) ["..."] : (...) -> (index, i1)`.
pub fn str_select<'c>(
    context: &'c Context,
    value: Value<'c, '_>,
    patterns: &[&str],
    location: Location<'c>,
) -> Operation<'c> {
    let attrs: Vec<Attribute<'c>> = patterns
        .iter()
        .map(|p| StringAttribute::new(context, p).into())
        .collect();
    OperationBuilder::new("reussir.str.select", location)
        .add_operands(&[value])
        .add_attributes(&[(
            Identifier::new(context, "patterns"),
            ArrayAttribute::new(context, &attrs).into(),
        )])
        .add_results(&[Type::index(context), IntegerType::new(context, 1).into()])
        .build()
        .expect("valid reussir.str.select")
}

/// `reussir.str.len (<str>) : index`.
pub fn str_len<'c>(
    context: &'c Context,
    value: Value<'c, '_>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.str.len", location)
        .add_operands(&[value])
        .add_results(&[Type::index(context)])
        .build()
        .expect("valid reussir.str.len")
}

/// `reussir.str.data_ptr (<str>) : <ptr>`: the pointer to the first byte.
pub fn str_data_ptr<'c>(
    context: &'c Context,
    value: Value<'c, '_>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.str.data_ptr", location)
        .add_operands(&[value])
        .add_results(&[crate::dialect::ty::generic_ptr(context)])
        .build()
        .expect("valid reussir.str.data_ptr")
}

/// `reussir.str.byte_at (<str>) [<index>] : i8` — bounds-checked, `0` when
/// out of bounds.
pub fn str_byte_at<'c>(
    context: &'c Context,
    value: Value<'c, '_>,
    index: Value<'c, '_>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.str.byte_at", location)
        .add_operands(&[value, index])
        .add_results(&[IntegerType::new(context, 8).into()])
        .build()
        .expect("valid reussir.str.byte_at")
}

/// `reussir.str.slice (<str>) [<offset>] : <result_type>` — clamped suffix.
pub fn str_slice<'c>(
    value: Value<'c, '_>,
    offset: Value<'c, '_>,
    result_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.str.slice", location)
        .add_operands(&[value, offset])
        .add_results(&[result_type])
        .build()
        .expect("valid reussir.str.slice")
}

/// `reussir.str.equal (<lhs>, <rhs>) : i1` — byte equality.
pub fn str_equal<'c>(
    context: &'c Context,
    lhs: Value<'c, '_>,
    rhs: Value<'c, '_>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.str.equal", location)
        .add_operands(&[lhs, rhs])
        .add_results(&[IntegerType::new(context, 1).into()])
        .build()
        .expect("valid reussir.str.equal")
}

/// `reussir.str.compare (<lhs>, <rhs>) : i32` — lexicographic, memcmp
/// convention (negative/zero/positive).
pub fn str_compare<'c>(
    context: &'c Context,
    lhs: Value<'c, '_>,
    rhs: Value<'c, '_>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.str.compare", location)
        .add_operands(&[lhs, rhs])
        .add_results(&[IntegerType::new(context, 32).into()])
        .build()
        .expect("valid reussir.str.compare")
}

/// `reussir.record.compound (<fields> : <types>) : <result_type>` — construct a
/// compound record value from its (declaration-ordered) field operands.
///
/// melior has no generated builder for the Reussir dialect, so the op is built
/// raw; the result type (the compound record type) must be supplied explicitly.
pub fn record_compound<'c>(
    fields: &[Value<'c, '_>],
    result_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.record.compound", location)
        .add_operands(fields)
        .add_results(&[result_type])
        .build()
        .expect("valid reussir.record.compound")
}

/// Create an RC array with a per-element initializer body.
/// Pass an empty region to leave the payload untouched for later initialization.
pub fn array_create<'c>(
    context: &'c Context,
    extents: &[Value<'c, '_>],
    body: Region<'c>,
    result_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.array.create", location)
        .add_operands(extents)
        .add_attributes(&[(
            Identifier::new(context, "operandSegmentSizes"),
            DenseI32ArrayAttribute::new(context, &[0, extents.len() as i32]).into(),
        )])
        .add_regions([body])
        .add_results(&[result_type])
        .build()
        .expect("valid reussir.array.create")
}

/// `reussir.rc.create value(<value> : <type>) : <result_type>` — box a value into
/// a fresh reference-counted pointer with an initial count of 1.
///
/// The op carries `AttrSizedOperandSegments` over its `[value, token, region]`
/// operand groups, but melior's generated builder leaves the required
/// `operandSegmentSizes` attribute unset, so it is constructed here with the
/// single value operand present (`[1, 0, 0]`). The allocation token and any
/// region are supplied later by the token-instantiation pass; the rc-create
/// fusion pass then folds an immediately preceding `record.compound` into this
/// op (`reussir.rc.create_compound`).
pub fn rc_create<'c>(
    context: &'c Context,
    value: Value<'c, '_>,
    result_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.rc.create", location)
        .add_operands(&[value])
        .add_attributes(&[(
            Identifier::new(context, "operandSegmentSizes"),
            DenseI32ArrayAttribute::new(context, &[1, 0, 0]).into(),
        )])
        .add_results(&[result_type])
        .build()
        .expect("valid reussir.rc.create")
}

/// `reussir.rc.create value(<value>) region(<region>) : <result_type>` — box a
/// value into a fresh region-allocated reference-counted pointer with `flex`
/// capability and an initial count of 1.
///
/// Like [`rc_create`] the op carries `AttrSizedOperandSegments` over its
/// `[value, token, region]` operand groups; here the value and region are
/// present and the token absent (`[1, 0, 1]`). Supplying the region is what
/// gives the result `flex` (region-local, mutable) capability — the region
/// patterns pass later attaches the box vtable and freezes the value to `rigid`
/// where it escapes its region.
pub fn rc_create_in_region<'c>(
    context: &'c Context,
    value: Value<'c, '_>,
    region: Value<'c, '_>,
    result_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.rc.create", location)
        .add_operands(&[value, region])
        .add_attributes(&[(
            Identifier::new(context, "operandSegmentSizes"),
            // operandSegmentSizes over [value, token, region]: value and region
            // present, token absent.
            DenseI32ArrayAttribute::new(context, &[1, 0, 1]).into(),
        )])
        .add_results(&[result_type])
        .build()
        .expect("valid reussir.rc.create with region")
}

/// `reussir.record.variant [<tag>] (<payload> : <payload_type>) : <variant_type>`
/// — wrap a case payload into a variant (enum) record value under its tag.
///
/// melior has no generated builder for the Reussir dialect, so the op is built
/// raw; the `tag` selector is an `index`-typed `IndexAttr` and the result is the
/// enum's variant record type. The payload is the `{enum}::{case}` compound
/// produced by a preceding `reussir.record.compound`; the rc-create fusion pass
/// later folds `variant` + `rc.create` into `reussir.rc.create_variant`.
pub fn record_variant<'c>(
    context: &'c Context,
    tag: usize,
    payload: Value<'c, '_>,
    result_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    let tag_attr = IntegerAttribute::new(Type::index(context), tag as i64).into();
    OperationBuilder::new("reussir.record.variant", location)
        .add_operands(&[payload])
        .add_attributes(&[(Identifier::new(context, "tag"), tag_attr)])
        .add_results(&[result_type])
        .build()
        .expect("valid reussir.record.variant")
}

/// `reussir.nullable.create (<value> : <type>)? : <result_type>` — wrap a
/// non-null pointer into a nullable pointer, or build a null pointer when no
/// value is given.
///
/// The op's `$ptr` operand is `Optional`, and melior's generated builder is
/// `(context, result_type, location)` — it exposes no parameter for that operand,
/// so it can only build the null pointer. The value-wrapping (non-null) case is
/// built raw here, adding the single pointer operand.
pub fn nullable_create<'c>(
    value: Option<Value<'c, '_>>,
    result_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    let mut builder = OperationBuilder::new("reussir.nullable.create", location);
    if let Some(value) = value {
        builder = builder.add_operands(&[value]);
    }
    builder
        .add_results(&[result_type])
        .build()
        .expect("valid reussir.nullable.create")
}

/// `reussir.nullable.dispatch (%v : nullable<…>) (-> <result_type>)?` — the
/// two-region null dispatch: the first (non-null) region's block takes the
/// unwrapped pointer as its argument, the second (null) region's block takes
/// none; both terminate with `reussir.scf.yield` ([`scf_yield`]).
///
/// Built raw like [`region_run`]: the op's result is `Optional`, which
/// melior's generated builder cannot express (`nullable.check`/`coerce` have
/// required results and use their generated builders directly).
pub fn nullable_dispatch<'c>(
    nullable: Value<'c, '_>,
    result_type: Option<Type<'c>>,
    non_null: Region<'c>,
    null: Region<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    let mut builder = OperationBuilder::new("reussir.nullable.dispatch", location)
        .add_operands(&[nullable])
        .add_regions_vec(vec![non_null, null]);
    if let Some(result_type) = result_type {
        builder = builder.add_results(&[result_type]);
    }
    builder.build().expect("valid reussir.nullable.dispatch")
}

/// `reussir.cell.create value(%value : T) : !reussir.rc<!reussir.cell<T>>`
/// — move a value into a fresh shared cell. The token-instantiation pass fills
/// the optional allocation token.
pub fn cell_create<'c>(
    value: Value<'c, '_>,
    result_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.cell.create", location)
        .add_operands(&[value])
        .add_results(&[result_type])
        .build()
        .expect("valid reussir.cell.create")
}

/// `reussir.cell.get(%cell : rc<cell<T>>) : T` — clone the stored value.
pub fn cell_get<'c>(
    cell: Value<'c, '_>,
    result_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.cell.get", location)
        .add_operands(&[cell])
        .add_results(&[result_type])
        .build()
        .expect("valid reussir.cell.get")
}

/// `reussir.cell.set(%value : T, %cell : rc<cell<T>>)` — replace the stored
/// value, consuming `value` and dropping the old element.
pub fn cell_set<'c>(
    value: Value<'c, '_>,
    cell: Value<'c, '_>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.cell.set", location)
        .add_operands(&[value, cell])
        .build()
        .expect("valid reussir.cell.set")
}

/// `reussir.cell.rmw(%cell) { ... }` — move the current element through a
/// single-block body and store its yielded replacement.
pub fn cell_rmw<'c>(
    cell: Value<'c, '_>,
    result_type: Option<Type<'c>>,
    body: Region<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    let mut builder = OperationBuilder::new("reussir.cell.rmw", location)
        .add_operands(&[cell])
        .add_regions([body]);
    if let Some(result_type) = result_type {
        builder = builder.add_results(&[result_type]);
    }
    builder.build().expect("valid reussir.cell.rmw")
}

/// `reussir.cell.rdlock(%cell) (-> R)? { body }` — run a read transaction
/// over an rwlock cell: the body receives a clone of the element (loaded and
/// acquired under the shared read lock), must consume it, and terminates
/// with `reussir.scf.yield` carrying the optional result.
pub fn cell_rdlock<'c>(
    cell: Value<'c, '_>,
    result_type: Option<Type<'c>>,
    body: Region<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    let mut builder = OperationBuilder::new("reussir.cell.rdlock", location)
        .add_operands(&[cell])
        .add_regions([body]);
    if let Some(result_type) = result_type {
        builder = builder.add_results(&[result_type]);
    }
    builder.build().expect("valid reussir.cell.rdlock")
}

/// `reussir.cell.yield(%replacement : T)` — terminate a cell RMW body.
pub fn cell_yield<'c>(
    replacement: Value<'c, '_>,
    output: Option<Value<'c, '_>>,
    location: Location<'c>,
) -> Operation<'c> {
    let mut operands = vec![replacement];
    if let Some(output) = output {
        operands.push(output);
    }
    OperationBuilder::new("reussir.cell.yield", location)
        .add_operands(&operands)
        .build()
        .expect("valid reussir.cell.yield")
}

/// `reussir.cell.in_use(%cell : rc<cell<T exclusive>>) : i1` — observe the
/// exclusive cell's in-use flag (true while an rmw body holds the element).
pub fn cell_in_use<'c>(
    cell: Value<'c, '_>,
    result_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.cell.in_use", location)
        .add_operands(&[cell])
        .add_results(&[result_type])
        .build()
        .expect("valid reussir.cell.in_use")
}

/// `reussir.region.run (-> <result_type>)? { <body> }` — execute a region scope.
///
/// The body region's single block takes a `!reussir.region` argument (the arena
/// handle) and terminates with `reussir.region.yield`. A `flex` value it yields
/// is frozen to `rigid` as it leaves the scope. melior's generated builder takes
/// only `(context, region, location)` — it exposes no result type for the op's
/// `Optional` result — so a value-yielding `region.run` is built raw here;
/// `result_type` is `None` for a region whose body yields nothing.
pub fn region_run<'c>(
    result_type: Option<Type<'c>>,
    body: Region<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    let mut builder = OperationBuilder::new("reussir.region.run", location);
    if let Some(result_type) = result_type {
        builder = builder.add_results(&[result_type]);
    }
    builder
        .add_regions([body])
        .build()
        .expect("valid reussir.region.run")
}

/// `reussir.region.yield (<value> : <type>)?` — terminate a `reussir.region.run`
/// body, optionally yielding a value out of the region.
///
/// melior's generated builder takes only `(context, location)` — it exposes no
/// parameter for the op's `Optional` value operand — so the op is built raw here
/// (the value is absent for a unit-typed region).
pub fn region_yield<'c>(value: Option<Value<'c, '_>>, location: Location<'c>) -> Operation<'c> {
    let mut builder = OperationBuilder::new("reussir.region.yield", location);
    if let Some(value) = value {
        builder = builder.add_operands(&[value]);
    }
    builder.build().expect("valid reussir.region.yield")
}

/// `reussir.closure.create -> <result_type> { body … }` — create a closure from
/// an inlined body region.
///
/// The result is a shared `!reussir.rc<closure<(inputs) -> output>>`; the body
/// region's single block takes the closure input types as arguments (captures
/// followed by parameters) and terminates with `reussir.closure.yield`. This
/// builds the *inlined* form — no allocation token (the token-instantiation pass
/// supplies it) and no outlined vtable (the closure-outlining pass generates the
/// evaluate/drop/clone functions and vtable from the body). The op has a custom
/// assembly format and no generated builder, so it is built raw here.
pub fn closure_create<'c>(
    result_type: Type<'c>,
    body: Region<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.closure.create", location)
        .add_regions([body])
        .add_results(&[result_type])
        .build()
        .expect("valid reussir.closure.create")
}

/// `reussir.closure.yield (<value> : <type>)?` — terminate a
/// `reussir.closure.create` body, yielding the closure's result (absent for a
/// closure with no output). Built raw like [`region_yield`] — the generated
/// builder exposes no parameter for the `Optional` value operand.
pub fn closure_yield<'c>(value: Option<Value<'c, '_>>, location: Location<'c>) -> Operation<'c> {
    let mut builder = OperationBuilder::new("reussir.closure.yield", location);
    if let Some(value) = value {
        builder = builder.add_operands(&[value]);
    }
    builder.build().expect("valid reussir.closure.yield")
}

/// `reussir.closure.apply (<arg> : <type>) to (<closure> : <type>) : <result_type>`
/// — supply one leading argument to a closure, yielding a closure with one fewer
/// input. Built raw here; the result type is the residual closure `rc` type.
pub fn closure_apply<'c>(
    arg: Value<'c, '_>,
    closure: Value<'c, '_>,
    result_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.closure.apply", location)
        .add_operands(&[arg, closure])
        .add_results(&[result_type])
        .build()
        .expect("valid reussir.closure.apply")
}

/// `reussir.closure.uniqify (<closure> : <type>) : <result_type>` — obtain a
/// uniquely-owned copy of a closure.
///
/// It expands (in the SCF-lowering pass) to an `rc.is_unique` guarded `scf.if`:
/// a uniquely-referenced closure is returned as-is, otherwise it is deep-cloned
/// (and the shared reference dropped). It must precede any `reussir.closure.apply`
/// on a closure that may be shared, because `apply` writes the argument into the
/// closure box *in place* — mutating a shared closure would corrupt every other
/// holder. The result type is the same closure `rc` type as the input.
pub fn closure_uniqify<'c>(
    closure: Value<'c, '_>,
    result_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.closure.uniqify", location)
        .add_operands(&[closure])
        .add_results(&[result_type])
        .build()
        .expect("valid reussir.closure.uniqify")
}

/// `reussir.closure.eval (<closure> : <type>) (: <result_type>)?` — evaluate a
/// fully-applied closure, producing its result (absent for a closure with no
/// output). Built raw here; `result_type` is `None` for a unit-returning closure.
pub fn closure_eval<'c>(
    closure: Value<'c, '_>,
    result_type: Option<Type<'c>>,
    location: Location<'c>,
) -> Operation<'c> {
    let mut builder = OperationBuilder::new("reussir.closure.eval", location);
    builder = builder.add_operands(&[closure]);
    if let Some(result_type) = result_type {
        builder = builder.add_results(&[result_type]);
    }
    builder.build().expect("valid reussir.closure.eval")
}

/// `reussir.record.extract (<record> : <type>) [<index>] : <field_type>` —
/// project a field out of a record value.
///
/// The field selector is an `index`-typed `IndexAttr`; the result type is the
/// projected field's type.
pub fn record_extract<'c>(
    context: &'c Context,
    record: Value<'c, '_>,
    index: usize,
    field_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    let index_attr = IntegerAttribute::new(Type::index(context), index as i64).into();
    OperationBuilder::new("reussir.record.extract", location)
        .add_operands(&[record])
        .add_attributes(&[(Identifier::new(context, "index"), index_attr)])
        .add_results(&[field_type])
        .build()
        .expect("valid reussir.record.extract")
}

/// `reussir.record.dispatch (<variant> : <ref>) (-> <result>)? { [t] -> {…} … }`
/// — dispatch on the tag of the variant record behind `variant`, one region per
/// entry of `tags_per_region` (region `i` handles the single tag
/// `tags_per_region[i]`, whose block takes a `!reussir.ref` to that case's
/// payload compound).
///
/// The `tagSets` attribute is an array of `DenseI64ArrayAttr`s (one tag set per
/// region), matching the dialect's expansion to `scf.index_switch`. Each region
/// must terminate with [`scf_yield`]. Built raw because the op's result is
/// `Optional`: melior's generated builder takes the regions and the `tagSets`
/// attribute but exposes no result-type parameter, so it can only build a void
/// dispatch — a value-producing `match` needs the result set here.
pub fn record_dispatch<'c>(
    context: &'c Context,
    variant: Value<'c, '_>,
    tags_per_region: &[i64],
    result_type: Option<Type<'c>>,
    regions: Vec<Region<'c>>,
    location: Location<'c>,
) -> Operation<'c> {
    let tag_sets: Vec<_> = tags_per_region
        .iter()
        .map(|&t| DenseI64ArrayAttribute::new(context, &[t]).into())
        .collect();
    let tag_sets_attr = ArrayAttribute::new(context, &tag_sets).into();
    let mut builder = OperationBuilder::new("reussir.record.dispatch", location)
        .add_operands(&[variant])
        .add_attributes(&[(Identifier::new(context, "tagSets"), tag_sets_attr)])
        .add_regions_vec(regions);
    if let Some(result_type) = result_type {
        builder = builder.add_results(&[result_type]);
    }
    builder.build().expect("valid reussir.record.dispatch")
}

/// `reussir.scf.yield (<value> : <type>)?` — terminate a `reussir.record.dispatch`
/// or `reussir.nullable.dispatch` arm, optionally yielding the dispatch's result.
/// Built raw like [`region_yield`]: the op's `$value` operand is `Optional`, and
/// melior's generated builder takes only `(context, location)` — it exposes no
/// parameter for it, so it can build only a value-less yield. The value is absent
/// for a void dispatch.
/// `reussir.array.with_unique_view (%array : rc) -> rc { ^bb(%view: memref): … }`
/// — uniqify (clone-if-shared) an rc array and run `body` with a mutable view
/// of its payload. With an empty terminating `reussir.scf.yield`, the op's
/// *implicit* result is the uniquified array itself, which is the only form
/// the code generator emits.
///
/// Built raw because the op has a custom assembly format and an optional
/// result, which the generated builder does not surface.
pub fn array_with_unique_view<'c>(
    array: Value<'c, '_>,
    result_type: Type<'c>,
    body: Region<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.array.with_unique_view", location)
        .add_operands(&[array])
        .add_results(&[result_type])
        .add_regions([body])
        .build()
        .expect("valid reussir.array.with_unique_view")
}

pub fn scf_yield<'c>(value: Option<Value<'c, '_>>, location: Location<'c>) -> Operation<'c> {
    let mut builder = OperationBuilder::new("reussir.scf.yield", location);
    if let Some(value) = value {
        builder = builder.add_operands(&[value]);
    }
    builder.build().expect("valid reussir.scf.yield")
}

/// `ub.poison : <ty>` — a poison value of arbitrary type, used to terminate a
/// provably-dead branch (a non-exhaustive gap or a redundant arm the frontend
/// already diagnosed) whose result is never observed.
///
/// Built raw because melior binds no `ub` dialect at all (it is neither a
/// `melior::dialect` module nor one of its `ods` dialects), so there is no
/// generated builder; the dialect itself is registered in the context as a
/// Reussir lowering dependency. The `value` attribute is the `#ub.poison`
/// poison attribute, parsed since melior cannot construct it directly.
pub fn poison<'c>(context: &'c Context, ty: Type<'c>, location: Location<'c>) -> Operation<'c> {
    let value = melior::ir::attribute::Attribute::parse(context, "#ub.poison")
        .expect("valid #ub.poison attribute");
    OperationBuilder::new("ub.poison", location)
        .add_attributes(&[(Identifier::new(context, "value"), value)])
        .add_results(&[ty])
        .build()
        .expect("valid ub.poison")
}

/// How a `ptr.load` or `ptr.store` accesses memory. `align` is zero for the
/// natural alignment of the accessed type.
#[derive(Clone, Copy, Default)]
pub struct PtrAccess {
    pub volatile: bool,
    pub nontemporal: bool,
    pub invariant: bool,
    pub invariant_group: bool,
    pub align: u64,
}

fn set_ptr_access(operation: &Operation<'_>, access: PtrAccess) {
    let set = unsafe {
        sys::reussirPtrAccessSetFlags(
            operation.to_raw(),
            access.volatile,
            access.nontemporal,
            access.invariant,
            access.invariant_group,
            access.align,
        )
    };
    assert!(set, "access flags apply to a `ptr.load` or `ptr.store`");
}

/// `ptr.load %ptr : <ptr> -> <result_type>`, with the flags of `access`.
pub fn ptr_load<'c>(
    ptr: Value<'c, '_>,
    result_type: Type<'c>,
    access: PtrAccess,
    location: Location<'c>,
) -> Operation<'c> {
    let operation = OperationBuilder::new("ptr.load", location)
        .add_operands(&[ptr])
        .add_results(&[result_type])
        .build()
        .expect("valid ptr.load");
    set_ptr_access(&operation, access);
    operation
}

/// `ptr.store %value, %ptr : <value>, <ptr>`, with the flags of `access`.
pub fn ptr_store<'c>(
    value: Value<'c, '_>,
    ptr: Value<'c, '_>,
    access: PtrAccess,
    location: Location<'c>,
) -> Operation<'c> {
    let operation = OperationBuilder::new("ptr.store", location)
        .add_operands(&[value, ptr])
        .build()
        .expect("valid ptr.store");
    set_ptr_access(&operation, access);
    operation
}

pub use sys::ReussirPtrAddFlags as PtrAddFlags;

/// `ptr.masked_load %ptr, %mask, %passthrough : <ptr> -> <vector>`: the
/// lanes whose mask bit is set are read; the others take `passthrough`.
pub fn ptr_masked_load<'c>(
    ptr: Value<'c, '_>,
    mask: Value<'c, '_>,
    passthrough: Value<'c, '_>,
    align: u64,
    location: Location<'c>,
) -> Operation<'c> {
    let operation = OperationBuilder::new("ptr.masked_load", location)
        .add_operands(&[ptr, mask, passthrough])
        .add_results(&[passthrough.r#type()])
        .build()
        .expect("valid ptr.masked_load");
    let access = PtrAccess {
        align,
        ..PtrAccess::default()
    };
    set_ptr_access(&operation, access);
    operation
}

/// `ptr.masked_store %value, %ptr, %mask : <vector>, <ptr>`: the lanes
/// whose mask bit is set are written.
pub fn ptr_masked_store<'c>(
    value: Value<'c, '_>,
    ptr: Value<'c, '_>,
    mask: Value<'c, '_>,
    align: u64,
    location: Location<'c>,
) -> Operation<'c> {
    let operation = OperationBuilder::new("ptr.masked_store", location)
        .add_operands(&[value, ptr, mask])
        .build()
        .expect("valid ptr.masked_store");
    let access = PtrAccess {
        align,
        ..PtrAccess::default()
    };
    set_ptr_access(&operation, access);
    operation
}

/// `ptr.gather %ptrs, %mask, %passthrough : <ptrs> -> <vector>`: each set
/// lane is read through its own pointer.
pub fn ptr_gather<'c>(
    ptrs: Value<'c, '_>,
    mask: Value<'c, '_>,
    passthrough: Value<'c, '_>,
    align: u64,
    location: Location<'c>,
) -> Operation<'c> {
    let operation = OperationBuilder::new("ptr.gather", location)
        .add_operands(&[ptrs, mask, passthrough])
        .add_results(&[passthrough.r#type()])
        .build()
        .expect("valid ptr.gather");
    let access = PtrAccess {
        align,
        ..PtrAccess::default()
    };
    set_ptr_access(&operation, access);
    operation
}

/// `ptr.scatter %value, %ptrs, %mask : <vector>, <ptrs>`: each set lane is
/// written through its own pointer.
pub fn ptr_scatter<'c>(
    value: Value<'c, '_>,
    ptrs: Value<'c, '_>,
    mask: Value<'c, '_>,
    align: u64,
    location: Location<'c>,
) -> Operation<'c> {
    let operation = OperationBuilder::new("ptr.scatter", location)
        .add_operands(&[value, ptrs, mask])
        .build()
        .expect("valid ptr.scatter");
    let access = PtrAccess {
        align,
        ..PtrAccess::default()
    };
    set_ptr_access(&operation, access);
    operation
}

/// `reussir.ptr.expand_load %ptr, %mask, %passthrough`.
pub fn ptr_expand_load<'c>(
    ptr: Value<'c, '_>,
    mask: Value<'c, '_>,
    passthrough: Value<'c, '_>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.ptr.expand_load", location)
        .add_operands(&[ptr, mask, passthrough])
        .add_results(&[passthrough.r#type()])
        .build()
        .expect("valid reussir.ptr.expand_load")
}

/// `reussir.ptr.compress_store %value, %ptr, %mask`.
pub fn ptr_compress_store<'c>(
    value: Value<'c, '_>,
    ptr: Value<'c, '_>,
    mask: Value<'c, '_>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.ptr.compress_store", location)
        .add_operands(&[value, ptr, mask])
        .build()
        .expect("valid reussir.ptr.compress_store")
}

/// `ptr.ptr_add <flags> %base, %offset : <ptr>, <offset>`: `base` moved by
/// `offset` bytes.
pub fn ptr_add<'c>(
    base: Value<'c, '_>,
    offset: Value<'c, '_>,
    flags: PtrAddFlags,
    location: Location<'c>,
) -> Operation<'c> {
    let operation = OperationBuilder::new("ptr.ptr_add", location)
        .add_operands(&[base, offset])
        .add_results(&[base.r#type()])
        .build()
        .expect("valid ptr.ptr_add");
    let set = unsafe { sys::reussirPtrAddSetFlags(operation.to_raw(), flags) };
    assert!(set, "flags apply to a `ptr.ptr_add`");
    operation
}

/// `ptr.ptr_diff %lhs, %rhs : <ptr> -> <result_type>`: `lhs - rhs` in bytes.
pub fn ptr_diff<'c>(
    lhs: Value<'c, '_>,
    rhs: Value<'c, '_>,
    result_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("ptr.ptr_diff", location)
        .add_operands(&[lhs, rhs])
        .add_results(&[result_type])
        .build()
        .expect("valid ptr.ptr_diff")
}

/// `ptr.type_offset <element> : <result_type>`.
pub fn ptr_type_offset<'c>(
    context: &'c Context,
    element: Type<'c>,
    result_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("ptr.type_offset", location)
        .add_attributes(&[(
            Identifier::new(context, "elementType"),
            TypeAttribute::new(element).into(),
        )])
        .add_results(&[result_type])
        .build()
        .expect("valid ptr.type_offset")
}

/// `memref.memory_space_cast %view : <memref> to <result_type>`.
pub fn memref_memory_space_cast<'c>(
    view: Value<'c, '_>,
    result_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("memref.memory_space_cast", location)
        .add_operands(&[view])
        .add_results(&[result_type])
        .build()
        .expect("valid memref.memory_space_cast")
}

/// `ptr.to_ptr %value : <ptr-like> -> <ptr_type>`: the pointer of a
/// pointer-like value in the same memory space.
pub fn ptr_to_ptr<'c>(
    value: Value<'c, '_>,
    ptr_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("ptr.to_ptr", location)
        .add_operands(&[value])
        .add_results(&[ptr_type])
        .build()
        .expect("valid ptr.to_ptr")
}

/// `reussir.ptr.addr %ptr : <ptr> -> <result_type>`.
pub fn ptr_addr<'c>(
    ptr: Value<'c, '_>,
    result_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.ptr.addr", location)
        .add_operands(&[ptr])
        .add_results(&[result_type])
        .build()
        .expect("valid reussir.ptr.addr")
}

/// `reussir.ptr.expose_addr %ptr : <ptr> -> <result_type>`.
pub fn ptr_expose_addr<'c>(
    ptr: Value<'c, '_>,
    result_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.ptr.expose_addr", location)
        .add_operands(&[ptr])
        .add_results(&[result_type])
        .build()
        .expect("valid reussir.ptr.expose_addr")
}

/// `reussir.ptr.launder_invariant_group %ptr : <ptr>`.
pub fn ptr_launder_invariant_group<'c>(
    ptr: Value<'c, '_>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.ptr.launder_invariant_group", location)
        .add_operands(&[ptr])
        .add_results(&[ptr.r#type()])
        .build()
        .expect("valid reussir.ptr.launder_invariant_group")
}

/// `reussir.ptr.strip_invariant_group %ptr : <ptr>`.
pub fn ptr_strip_invariant_group<'c>(ptr: Value<'c, '_>, location: Location<'c>) -> Operation<'c> {
    OperationBuilder::new("reussir.ptr.strip_invariant_group", location)
        .add_operands(&[ptr])
        .add_results(&[ptr.r#type()])
        .build()
        .expect("valid reussir.ptr.strip_invariant_group")
}

/// `reussir.ptr.from_addr %addr : <addr> -> <ptr_type>`.
pub fn ptr_from_addr<'c>(
    addr: Value<'c, '_>,
    ptr_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.ptr.from_addr", location)
        .add_operands(&[addr])
        .add_results(&[ptr_type])
        .build()
        .expect("valid reussir.ptr.from_addr")
}

/// `vector.broadcast %value : <scalar or vector> to <vector_type>`.
pub fn vector_broadcast<'c>(
    value: Value<'c, '_>,
    vector_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("vector.broadcast", location)
        .add_operands(&[value])
        .add_results(&[vector_type])
        .build()
        .expect("valid vector.broadcast")
}

/// `vector.step : <vector_type>`: the lanes 0, 1, 2, ....
pub fn vector_step<'c>(vector_type: Type<'c>, location: Location<'c>) -> Operation<'c> {
    OperationBuilder::new("vector.step", location)
        .add_results(&[vector_type])
        .build()
        .expect("valid vector.step")
}

/// `vector.vscale`: the scalable factor, as an `index`.
pub fn vector_vscale<'c>(context: &'c Context, location: Location<'c>) -> Operation<'c> {
    OperationBuilder::new("vector.vscale", location)
        .add_results(&[Type::index(context)])
        .build()
        .expect("valid vector.vscale")
}

/// `vector.from_elements %elements... : <vector_type>`.
pub fn vector_from_elements<'c>(
    elements: &[Value<'c, '_>],
    vector_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("vector.from_elements", location)
        .add_operands(elements)
        .add_results(&[vector_type])
        .build()
        .expect("valid vector.from_elements")
}

/// A position of `vector.extract` or `vector.insert`: a constant, or an
/// `index` value.
#[derive(Clone, Copy)]
pub enum VectorPosition<'c, 'a> {
    Static(i64),
    Dynamic(Value<'c, 'a>),
}

/// Set an inherent attribute that the operation keeps in its properties,
/// which the generic operation builder does not fill.
fn set_inherent_attribute(operation: &Operation<'_>, name: &str, value: Attribute<'_>) {
    use melior::ir::attribute::AttributeLike;
    let name = melior::StringRef::new(name);
    unsafe {
        mlir_sys::mlirOperationSetInherentAttributeByName(
            operation.to_raw(),
            name.to_raw(),
            value.to_raw(),
        );
    }
}

pub use sys::ReussirVectorCombiningKind as VectorCombiningKind;

/// `vector.reduction <kind>, %vector : <vector> into <result_type>`: the
/// lanes of a one-dimensional vector combined into a scalar. `fastmath` is
/// an `#arith.fastmath` attribute.
pub fn vector_reduction<'c>(
    context: &'c Context,
    kind: VectorCombiningKind,
    vector: Value<'c, '_>,
    result_type: Type<'c>,
    fastmath: Option<Attribute<'c>>,
    location: Location<'c>,
) -> Operation<'c> {
    let operation = OperationBuilder::new("vector.reduction", location)
        .add_operands(&[vector])
        .add_results(&[result_type])
        .build()
        .expect("valid vector.reduction");
    let kind = unsafe {
        Attribute::from_raw(sys::reussirVectorCombiningKindAttrGet(
            context.to_raw(),
            kind,
        ))
    };
    set_inherent_attribute(&operation, "kind", kind);
    if let Some(fastmath) = fastmath {
        set_inherent_attribute(&operation, "fastmath", fastmath);
    }
    operation
}

/// A `vector` dialect operation with the given operands, result types and
/// inherent attributes; the attributes are set after construction, because
/// the operations keep them in their properties.
pub fn vector_operation<'c>(
    name: &str,
    operands: &[Value<'c, '_>],
    results: &[Type<'c>],
    attributes: &[(&str, Attribute<'c>)],
    location: Location<'c>,
) -> Operation<'c> {
    let operation = OperationBuilder::new(name, location)
        .add_operands(operands)
        .add_results(results)
        .build()
        .expect("valid vector operation");
    for &(name, value) in attributes {
        set_inherent_attribute(&operation, name, value);
    }
    operation
}

/// `array<i64: values...>`, the positions of several `vector` operations.
pub fn dense_i64_array<'c>(context: &'c Context, values: &[i64]) -> Attribute<'c> {
    DenseI64ArrayAttribute::new(context, values).into()
}

/// `[values...]` as an array of `i64` attributes.
pub fn i64_array<'c>(context: &'c Context, values: &[i64]) -> Attribute<'c> {
    let i64_ty = IntegerType::new(context, 64).into();
    let elements: Vec<Attribute<'c>> = values
        .iter()
        .map(|&v| IntegerAttribute::new(i64_ty, v).into())
        .collect();
    ArrayAttribute::new(context, &elements).into()
}

/// `value : i64` as an attribute.
pub fn i64_attribute<'c>(context: &'c Context, value: i64) -> Attribute<'c> {
    IntegerAttribute::new(IntegerType::new(context, 64).into(), value).into()
}

/// `vector.create_mask %counts... : <mask_type>`: in each dimension the
/// leading `count` lanes are set. The counts are `index` values.
pub fn vector_create_mask<'c>(
    counts: &[Value<'c, '_>],
    mask_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("vector.create_mask", location)
        .add_operands(counts)
        .add_results(&[mask_type])
        .build()
        .expect("valid vector.create_mask")
}

/// `vector.shape_cast %vector : <vector> to <result_type>`.
pub fn vector_shape_cast<'c>(
    vector: Value<'c, '_>,
    result_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("vector.shape_cast", location)
        .add_operands(&[vector])
        .add_results(&[result_type])
        .build()
        .expect("valid vector.shape_cast")
}

fn set_static_position(operation: &Operation<'_>, positions: Attribute<'_>) {
    set_inherent_attribute(operation, "static_position", positions);
}

fn vector_positions<'c, 'a>(
    context: &'c Context,
    positions: &[VectorPosition<'c, 'a>],
) -> (Vec<Value<'c, 'a>>, Attribute<'c>) {
    // The sentinel `vector.extract` and `vector.insert` use for a dynamic
    // position in `static_position`.
    const DYNAMIC: i64 = i64::MIN;
    let dynamic = positions
        .iter()
        .filter_map(|p| match *p {
            VectorPosition::Dynamic(value) => Some(value),
            VectorPosition::Static(_) => None,
        })
        .collect();
    let fixed: Vec<i64> = positions
        .iter()
        .map(|p| match *p {
            VectorPosition::Static(n) => n,
            VectorPosition::Dynamic(_) => DYNAMIC,
        })
        .collect();
    (dynamic, DenseI64ArrayAttribute::new(context, &fixed).into())
}

/// `vector.extract %source[positions] : <result_type> from <vector>`.
pub fn vector_extract<'c, 'a>(
    context: &'c Context,
    source: Value<'c, 'a>,
    positions: &[VectorPosition<'c, 'a>],
    result_type: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    let (dynamic, fixed) = vector_positions(context, positions);
    let operation = OperationBuilder::new("vector.extract", location)
        .add_operands(&[source])
        .add_operands(&dynamic)
        .add_results(&[result_type])
        .build()
        .expect("valid vector.extract");
    set_static_position(&operation, fixed);
    operation
}

/// `vector.insert %value, %dest[positions] : <value> into <vector>`.
pub fn vector_insert<'c, 'a>(
    context: &'c Context,
    value: Value<'c, 'a>,
    dest: Value<'c, 'a>,
    positions: &[VectorPosition<'c, 'a>],
    location: Location<'c>,
) -> Operation<'c> {
    let (dynamic, fixed) = vector_positions(context, positions);
    let operation = OperationBuilder::new("vector.insert", location)
        .add_operands(&[value, dest])
        .add_operands(&dynamic)
        .add_results(&[dest.r#type()])
        .build()
        .expect("valid vector.insert");
    set_static_position(&operation, fixed);
    operation
}

/// `reussir.ptr.mask %ptr, %mask : <ptr>, <mask>`.
pub fn ptr_mask<'c>(
    ptr: Value<'c, '_>,
    mask: Value<'c, '_>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("reussir.ptr.mask", location)
        .add_operands(&[ptr, mask])
        .add_results(&[ptr.r#type()])
        .build()
        .expect("valid reussir.ptr.mask")
}
