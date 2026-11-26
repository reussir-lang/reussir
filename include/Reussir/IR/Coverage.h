#ifndef REUSSIR_IR_COVERAGE_H
#define REUSSIR_IR_COVERAGE_H

#include <mlir/IR/BuiltinOps.h>

namespace reussir {

std::string coverageScopeKey(llvm::ArrayRef<llvm::StringRef> includes,
                             llvm::ArrayRef<llvm::StringRef> excludes);
bool coverageEnabled(mlir::Operation *op);
bool sameCoverageScope(mlir::Operation *lhs, mlir::Operation *rhs);
void prepareCoverage(mlir::ModuleOp module);
void inheritCoverage(mlir::Operation *source, mlir::Operation *function);
void excludeCoverage(mlir::Operation *function);

// Whether `op` adds to a profile counter, as `llvm::InstrProfIncrementInst`
// does. Only the profile runtime reads the counter, so the program cannot
// see the op.
bool isCoverageIncrement(mlir::Operation *op);

} // namespace reussir

#endif
