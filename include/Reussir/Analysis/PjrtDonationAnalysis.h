// Part of the Reussir Project, dual licensed under Apache-2.0 OR MIT.
#pragma once

#include <memory>
#include <mlir/IR/BuiltinOps.h>
#include <mlir/IR/Value.h>

namespace reussir {
class ReussirPJRTJitCallOp;

class PjrtDonationAnalysis {
public:
  enum class Decision { Forbidden, Guarded, Unconditional };
  explicit PjrtDonationAnalysis(mlir::ModuleOp module,
                                uint64_t maxWork = 1000000);
  ~PjrtDonationAnalysis();
  Decision analyze(mlir::Value array, ReussirPJRTJitCallOp call);
  uint64_t getWork() const;
  bool exhausted() const;

private:
  struct Impl;
  std::unique_ptr<Impl> impl;
};
} // namespace reussir
