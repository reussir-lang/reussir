//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
#include "Reussir/IR/RecordDrop.h"
#include "Reussir/IR/ReussirDialect.h"
#include <gtest/gtest.h>

namespace reussir {
namespace {
class RecordDropTest : public testing::Test {
protected:
  mlir::MLIRContext context;
  RecordDropTest() { context.getOrLoadDialect<ReussirDialect>(); }
  RecordType declare(llvm::StringRef name,
                     RecordKind kind = RecordKind::compound) {
    return RecordType::get(&context, mlir::StringAttr::get(&context, name),
                           kind);
  }
  void complete(RecordType record, llvm::ArrayRef<mlir::Type> members,
                Capability capability = Capability::shared) {
    llvm::SmallVector<bool> fields(members.size(), false);
    record.complete(members, fields, capability, false);
  }
  mlir::Type scalar() { return mlir::IntegerType::get(&context, 32); }
};

TEST_F(RecordDropTest, AcyclicBranchingIsDirect) {
  auto leaf = declare("Leaf");
  complete(leaf, {scalar()});
  auto pair = declare("Pair");
  complete(pair, {leaf, leaf});
  EXPECT_EQ(classifyRecordDrop(leaf), RecordDropKind::Direct);
  EXPECT_EQ(classifyRecordDrop(pair), RecordDropKind::Direct);
}

TEST_F(RecordDropTest, ChainAndNullableChainUseTailLoop) {
  auto chain = declare("Chain");
  complete(chain, {scalar(), chain});
  EXPECT_EQ(classifyRecordDrop(chain), RecordDropKind::TailLoop);
  auto nullable = declare("NullableChain");
  auto link =
      RcType::get(&context, nullable, Capability::shared, AtomicKind::normal);
  complete(nullable, {NullableType::get(&context, link), scalar()});
  EXPECT_EQ(classifyRecordDrop(nullable), RecordDropKind::TailLoop);
}

TEST_F(RecordDropTest, TreeUsesWorklist) {
  auto tree = declare("Tree");
  complete(tree, {tree, tree});
  EXPECT_EQ(classifyRecordDrop(tree), RecordDropKind::Worklist);
}

TEST_F(RecordDropTest, MutualRecursionUsesTailLoop) {
  auto a = declare("A");
  auto b = declare("B");
  complete(a, {b});
  complete(b, {a});
  EXPECT_EQ(classifyRecordDrop(a), RecordDropKind::TailLoop);
  EXPECT_EQ(classifyRecordDrop(b), RecordDropKind::TailLoop);
}

TEST_F(RecordDropTest, MutualBranchingUsesWorklist) {
  auto a = declare("A");
  auto b = declare("B");
  complete(a, {b});
  complete(b, {a, a});
  EXPECT_EQ(classifyRecordDrop(a), RecordDropKind::Worklist);
  EXPECT_EQ(classifyRecordDrop(b), RecordDropKind::Worklist);
}

TEST_F(RecordDropTest, AlternativesAreNotSimultaneousChildren) {
  auto variant = declare("Variant", RecordKind::variant);
  auto left = declare("Left");
  auto right = declare("Right");
  complete(left, {scalar(), variant}, Capability::value);
  complete(right, {variant, scalar()}, Capability::value);
  complete(variant, {scalar(), left, right});
  EXPECT_EQ(classifyRecordDrop(variant), RecordDropKind::TailLoop);
}

TEST_F(RecordDropTest, ValueVariantWithBoxedConstructorUsesTailLoop) {
  auto list = declare("List", RecordKind::variant);
  auto cons = declare("Cons");
  complete(list, {scalar(), cons}, Capability::value);
  complete(cons, {scalar(), list});
  EXPECT_EQ(classifyRecordDrop(list), RecordDropKind::TailLoop);
  EXPECT_EQ(classifyRecordDrop(cons), RecordDropKind::TailLoop);
}

TEST_F(RecordDropTest, NestedNontrivialSuffixUsesWorklist) {
  auto leaf = declare("Leaf");
  complete(leaf, {scalar()});
  auto suffix = declare("Suffix");
  complete(suffix, {leaf}, Capability::value);
  auto chain = declare("Chain");
  complete(chain, {chain, suffix});
  EXPECT_EQ(classifyRecordDrop(chain), RecordDropKind::Worklist);
}

TEST_F(RecordDropTest, InlinePayloadCanHideBranching) {
  auto variant = declare("Tree", RecordKind::variant);
  auto payload = declare("Pair");
  complete(payload, {variant, variant}, Capability::value);
  complete(variant, {scalar(), payload});
  EXPECT_EQ(classifyRecordDrop(variant), RecordDropKind::Worklist);
}

TEST_F(RecordDropTest, AcyclicOwnerOfTreeIsDirect) {
  auto tree = declare("Tree");
  complete(tree, {tree, tree});
  auto owner = declare("Owner");
  complete(owner, {tree});
  EXPECT_EQ(classifyRecordDrop(owner), RecordDropKind::Direct);
}

TEST_F(RecordDropTest, NonTailDropUsesWorklist) {
  auto leaf = declare("Leaf");
  complete(leaf, {scalar()});
  auto chain = declare("Chain");
  complete(chain, {chain, leaf});
  EXPECT_EQ(classifyRecordDrop(chain), RecordDropKind::Worklist);
  auto tail = declare("Tail");
  complete(tail, {leaf, tail});
  EXPECT_EQ(classifyRecordDrop(tail), RecordDropKind::TailLoop);
}

TEST_F(RecordDropTest, TrivialInlineSuffixDoesNotPreventTailLoop) {
  auto value = declare("Value");
  complete(value, {scalar()}, Capability::value);
  auto chain = declare("Chain");
  complete(chain, {chain, value});
  EXPECT_EQ(classifyRecordDrop(chain), RecordDropKind::TailLoop);
}

TEST_F(RecordDropTest, BorrowedFieldsDoNotAddDropEdges) {
  auto record = declare("Borrowed");
  record.complete({record, record}, {true, true}, Capability::shared, false);
  EXPECT_EQ(classifyRecordDrop(record), RecordDropKind::Direct);
}

TEST_F(RecordDropTest, IncompleteTypesAreNotCached) {
  auto record = declare("Incomplete");
  EXPECT_EQ(classifyRecordDrop(record), std::nullopt);
  complete(record, {record});
  EXPECT_EQ(classifyRecordDrop(record), RecordDropKind::TailLoop);
}

TEST_F(RecordDropTest, OpaqueAtomicAndRegionalMembersAreClassified) {
  auto record = declare("ClosureOwner");
  complete(record, {ClosureType::get(&context, {}, scalar())});
  EXPECT_EQ(classifyRecordDrop(record), RecordDropKind::Direct);
  auto atomic = declare("Atomic");
  complete(atomic,
           {NullableType::get(&context,
                              RcType::get(&context, atomic, Capability::shared,
                                          AtomicKind::atomic))});
  EXPECT_EQ(classifyRecordDrop(atomic), RecordDropKind::TailLoop);
  auto regional = declare("Regional");
  complete(regional, {regional}, Capability::regional);
  EXPECT_EQ(classifyRecordDrop(regional), RecordDropKind::Direct);
}

TEST_F(RecordDropTest, RecursiveArraysUseTheirElementMultiplicity) {
  const int64_t extents[] = {0, 1, 2, mlir::ShapedType::kDynamic};
  for (int64_t extent : extents) {
    auto record = declare("Array" + std::to_string(extent));
    auto link = RcType::get(&context, record, Capability::shared);
    complete(record, {ArrayType::get(&context, {extent}, link)});
    auto expected = extent == 0   ? RecordDropKind::Direct
                    : extent == 1 ? RecordDropKind::TailLoop
                                  : RecordDropKind::Worklist;
    EXPECT_EQ(classifyRecordDrop(record), expected);
  }
}

TEST_F(RecordDropTest, RecursiveCellsAccountForLockCleanup) {
  for (auto kind : {CellKind::plain, CellKind::mutex, CellKind::flatlock,
                    CellKind::rwlock}) {
    auto record = declare(stringifyCellKind(kind));
    auto link =
        RcType::get(&context, record, Capability::shared, AtomicKind::atomic);
    complete(record, {CellType::get(&context, link, kind)});
    EXPECT_EQ(classifyRecordDrop(record), kind == CellKind::plain
                                              ? RecordDropKind::TailLoop
                                              : RecordDropKind::Worklist);
  }
}

TEST_F(RecordDropTest, ForeignCleanupAfterRecursionUsesWorklist) {
  auto record = declare("WithForeignCleanup");
  auto foreign =
      FFIObjectType::get(&context, mlir::StringAttr::get(&context, "Foreign"),
                         mlir::FlatSymbolRefAttr::get(&context, "cleanup"));
  auto link = NullableType::get(
      &context, RcType::get(&context, foreign, Capability::shared));
  complete(record, {record, link});
  EXPECT_EQ(classifyRecordDrop(record), RecordDropKind::Worklist);
  auto owner = declare("ForeignOwner");
  complete(owner, {link});
  EXPECT_EQ(classifyRecordDrop(owner), RecordDropKind::Direct);
}

TEST_F(RecordDropTest, ScalarBoxesArraysAndCellsAreDirect) {
  auto record = declare("Containers");
  complete(record,
           {NullableType::get(
                &context, RcType::get(&context, scalar(), Capability::shared)),
            ArrayType::get(&context, {2}, scalar()),
            CellType::get(&context, scalar(), CellKind::plain)});
  EXPECT_EQ(classifyRecordDrop(record), RecordDropKind::Direct);
}

TEST_F(RecordDropTest, IllFormedRecordsHaveNoClassification) {
  EXPECT_EQ(classifyRecordDrop(RecordType{}), std::nullopt);
  auto cycle = declare("InlineCycle");
  complete(cycle, {cycle}, Capability::value);
  EXPECT_EQ(classifyRecordDrop(cycle), std::nullopt);
  auto incomplete = declare("IncompleteChild");
  auto owner = declare("Owner");
  complete(owner, {incomplete});
  EXPECT_EQ(classifyRecordDrop(owner), std::nullopt);
}

TEST_F(RecordDropTest, SharedAcyclicGraphDoesNotExpandPaths) {
  auto record = declare("Leaf");
  complete(record, {scalar()});
  for (unsigned i = 0; i < 64; ++i) {
    auto parent = declare("Level" + std::to_string(i));
    complete(parent, {record, record});
    record = parent;
  }
  EXPECT_EQ(classifyRecordDrop(record), RecordDropKind::Direct);
}
} // namespace
} // namespace reussir
