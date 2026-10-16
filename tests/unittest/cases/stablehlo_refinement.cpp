//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file tests the C ABI of runtime StableHLO shape refinement.
///
//===----------------------------------------------------------------------===//

#ifdef REUSSIR_ENABLE_OPENXLA
#include "reussir_stablehlo.h"
#include <gtest/gtest.h>
#include <string>
#include <vector>

namespace {
struct Collected {
  std::string bytes, diagnostic;
  std::vector<std::vector<int64_t>> dimensions;
  std::vector<uint32_t> elementBits;
};

void collectBytes(const uint8_t *data, size_t size, void *user) {
  static_cast<Collected *>(user)->bytes.append(
      reinterpret_cast<const char *>(data), size);
}

void collectDiagnostic(const uint8_t *data, size_t size, void *user) {
  static_cast<Collected *>(user)->diagnostic.append(
      reinterpret_cast<const char *>(data), size);
}

void collectResultType(const int64_t *data, size_t rank, uint32_t bits,
                       void *user) {
  auto *collected = static_cast<Collected *>(user);
  collected->dimensions.emplace_back(data, data + rank);
  collected->elementBits.push_back(bits);
}

uint32_t refine(const std::string &program,
                const std::vector<ReussirStablehloArgument> &arguments,
                Collected &collected) {
  return reussir_stablehlo_refine_shapes(
      reinterpret_cast<const uint8_t *>(program.data()), program.size(),
      arguments.data(), arguments.size(), collectBytes, collectResultType,
      collectDiagnostic, &collected);
}
} // namespace

TEST(StablehloRefinement, InvalidInputProducesOnlyDiagnostics) {
  Collected collected;
  EXPECT_EQ(reussir_stablehlo_refine_shapes(nullptr, 3, nullptr, 0,
                                            collectBytes, collectResultType,
                                            collectDiagnostic, &collected),
            REUSSIR_STABLEHLO_INVALID_INPUT);
  EXPECT_TRUE(collected.bytes.empty());
  EXPECT_TRUE(collected.dimensions.empty());
  EXPECT_FALSE(collected.diagnostic.empty());
}

TEST(StablehloRefinement, ReportsScalarAndRefinedResultTypes) {
  const std::string program = R"(module {
    func.func @main(%x: tensor<?xi16>, %s: tensor<i1>) -> (tensor<?xi16>, tensor<i1>) {
      %y = stablehlo.add %x, %x : tensor<?xi16>
      return %y, %s : tensor<?xi16>, tensor<i1>
    }
  })";
  int64_t extent = 5;
  Collected collected;
  EXPECT_EQ(refine(program,
                   {{&extent, 1, nullptr, 0, 0}, {nullptr, 0, nullptr, 0, 0}},
                   collected),
            REUSSIR_STABLEHLO_OK)
      << collected.diagnostic;
  EXPECT_FALSE(collected.bytes.empty());
  EXPECT_EQ(collected.dimensions, (std::vector<std::vector<int64_t>>{{5}, {}}));
  EXPECT_EQ(collected.elementBits, (std::vector<uint32_t>{16, 1}));
}
#endif
