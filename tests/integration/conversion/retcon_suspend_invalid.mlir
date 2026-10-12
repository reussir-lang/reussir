// RUN: %reussir-opt %s --split-input-file --verify-diagnostics

func.func private @child(!llvm.ptr) attributes {reussir.retcon_drop}
func.func @parent(%p: !llvm.ptr) {
  // expected-error @+1 {{requires a retcon destructor parent}}
  reussir.dtor.suspend @child(%p : !llvm.ptr)
  return
}

// -----

func.func private @child(!llvm.ptr)
func.func @parent(%p: !llvm.ptr) attributes {reussir.retcon_drop} {
  // expected-error @+1 {{requires a retcon destructor callee}}
  reussir.dtor.suspend @child(%p : !llvm.ptr)
  return
}

// -----

func.func private @child(i64) attributes {reussir.retcon_drop}
func.func @parent(%p: !llvm.ptr) attributes {reussir.retcon_drop} {
  // expected-error @+1 {{object type must match the destructor argument}}
  reussir.dtor.suspend @child(%p : !llvm.ptr)
  return
}
