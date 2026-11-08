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

} // namespace reussir

#endif
