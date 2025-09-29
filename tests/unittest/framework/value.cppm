module;

#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"

#include <gtest/gtest.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>

export module reussir.test.value;
import reussir.test;

namespace reussir {
export class ReussirValueTransformTest : public ReussirTest {
protected:
  template <typename F>
  void testValueAcquisition(std::string_view argType, F &&func)
    requires std::is_invocable_v<F, mlir::func::FuncOp>
  {
    constexpr llvm::StringRef moduleTemplate = R"(
      module attributes {{ dlti.dl_spec = #dlti.dl_spec<
        #dlti.dl_entry<i64, dense<64> : vector<2xi64>>, 
        #dlti.dl_entry<i8, dense<8> : vector<2xi64>>,
        #dlti.dl_entry<f128, dense<128> : vector<2xi64>>,
        #dlti.dl_entry<i32, dense<32> : vector<2xi64>>,
        #dlti.dl_entry<i16, dense<16> : vector<2xi64>>>
      }}
      {{
         func.func private @test(%value: {}) {{
              func.return
         }}
      }}
    )";
    withModule(std::format(moduleTemplate, argType),
               [func = std::forward<F>(func)](mlir::ModuleOp module) {
                 // std::forward<F>(func)(module.get());
                 auto funcOp =
                     llvm::cast<mlir::func::FuncOp>(module.getBody()->front());
                 mlir::OpBuilder builder(funcOp.getBody());
                 auto arg = funcOp.getBody().getArgument(0);
                 auto res =
                     emitOwnershipAcquisition(arg, builder, funcOp.getLoc());
                 EXPECT_TRUE(res.succeeded())
                     << "failed to acquire ownership\n";
                 func(funcOp);
               });
  }
};
} // namespace reussir
