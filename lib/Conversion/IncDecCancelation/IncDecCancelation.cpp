#include "Reussir/Conversion/IncDecCancelation.h"
#include "Reussir/IR/ReussirDialect.h"

#include <mlir/Pass/Pass.h>

namespace reussir {

#define GEN_PASS_DEF_REUSSIRINCDECCANCELATIONPASS
#include "Reussir/Conversion/Passes.h.inc"

//===----------------------------------------------------------------------===//
// IncDecCancelationPass
//===----------------------------------------------------------------------===//

namespace {
struct IncDecCancelationPass
    : public impl::ReussirIncDecCancelationPassBase<IncDecCancelationPass> {
  using Base::Base;
  void runOnOperation() override { runIncDecCancelation(getOperation()); }
};
} // namespace

void runIncDecCancelation(mlir::ModuleOp module) {
  // TODO: Implement inc/dec cancellation logic
}

} // namespace reussir
