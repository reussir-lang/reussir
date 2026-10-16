// Part of the Reussir Project, dual licensed under Apache-2.0 OR MIT.
#ifdef REUSSIR_ENABLE_OPENXLA
#include "../../../crates/reussir-stablehlo-sys/native/specialize.h"
#include <gtest/gtest.h>
#include <string>
#include <vector>
namespace {
struct Result {
  std::string bytes, error;
  std::vector<std::vector<int64_t>> shapes;
};
void bytes(const uint8_t *data, size_t size, void *user) {
  static_cast<Result *>(user)->bytes.append(
      reinterpret_cast<const char *>(data), size);
}
void error(const uint8_t *data, size_t size, void *user) {
  static_cast<Result *>(user)->error.append(
      reinterpret_cast<const char *>(data), size);
}
void shape(const int64_t *data, size_t size, void *user) {
  auto &result = static_cast<Result *>(user)->shapes.emplace_back();
  if (size)
    result.assign(data, data + size);
}
TEST(StablehloSpecialization, InvalidInputDoesNotProduceOutput) {
  Result result;
  EXPECT_EQ(reussir_stablehlo_specialize(nullptr, 3, nullptr, 0, bytes, shape,
                                         error, &result),
            REUSSIR_STABLEHLO_INVALID_INPUT);
  EXPECT_TRUE(result.bytes.empty());
  EXPECT_FALSE(result.error.empty());
}
TEST(StablehloSpecialization, ScalarAndDynamicResultShapes) {
  const std::string source = R"(module {
    func.func @main(%x: tensor<?xi32>, %s: tensor<i32>) -> (tensor<?xi32>, tensor<i32>) {
      %y = stablehlo.add %x, %x : tensor<?xi32>
      return %y, %s : tensor<?xi32>, tensor<i32>
    }
  })";
  int64_t dims[] = {5};
  ReussirStablehloArgument args[] = {{dims, 1, nullptr, 0, 0},
                                     {nullptr, 0, nullptr, 0, 0}};
  Result result;
  EXPECT_EQ(reussir_stablehlo_specialize(
                reinterpret_cast<const uint8_t *>(source.data()), source.size(),
                args, 2, bytes, shape, error, &result),
            REUSSIR_STABLEHLO_OK)
      << result.error;
  ASSERT_EQ(result.shapes.size(), 2u);
  EXPECT_EQ(result.shapes[0], std::vector<int64_t>{5});
  EXPECT_TRUE(result.shapes[1].empty());
  EXPECT_FALSE(result.bytes.empty());
}
} // namespace
#endif
