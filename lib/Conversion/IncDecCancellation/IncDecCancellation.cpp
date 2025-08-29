#include "Reussir/Conversion/IncDecCancellation.h"
#include "Reussir/IR/ReussirDialect.h"

#include <mlir/Pass/Pass.h>

namespace reussir {

#define GEN_PASS_DEF_REUSSIRINCDECCANCELLATIONPASS
#include "Reussir/Conversion/Passes.h.inc"

//===----------------------------------------------------------------------===//
// IncDecCancellationPass
//===----------------------------------------------------------------------===//

namespace {
struct IncDecCancellationPass
    : public impl::ReussirIncDecCancellationPassBase<IncDecCancellationPass> {
  using Base::Base;
  void runOnOperation() override { runIncDecCancellation(getOperation()); }
};
} // namespace

void runIncDecCancellation(mlir::ModuleOp module) {
  // TODO: Implement inc/dec cancellation logic
}

} // namespace reussir
