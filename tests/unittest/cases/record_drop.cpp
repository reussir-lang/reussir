//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
#include "Reussir/Conversion/Blake3Symbol.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirTypes.h"
#include <gtest/gtest.h>
#include <llvm/Support/raw_ostream.h>
#ifdef REUSSIR_ENABLE_RCGMP
#include "RCGMP/IR/RCGMPDialect.h"
#include "RCGMP/IR/RCGMPTypes.h"
#endif

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
  EXPECT_EQ(leaf.classifyRecordDrop(), RecordDropKind::Direct);
  EXPECT_EQ(pair.classifyRecordDrop(), RecordDropKind::Direct);
  EXPECT_FALSE(pair.hasObservableCleanup());
}

TEST_F(RecordDropTest, ForwardArrayWalkSymbolIsStable) {
  auto array =
      ArrayType::get(&context, {2}, mlir::IntegerType::get(&context, 64));
  auto signature =
      mlir::FunctionType::get(&context, {RefType::get(&context, array)}, {});
  std::string text;
  llvm::raw_string_ostream stream(text);
  printTypeWithRecordBodiesOnce(signature, stream);
  EXPECT_EQ(mangledBlake3Symbol("REUSSIR_DROP_walk_forward", text),
            "_RNvC25REUSSIR_DROP_walk_forward43"
            "CZhH4niIQwWWLjrUf4yvQevfo0YrPlMeqRG77pTF5I4");
}

TEST_F(RecordDropTest, ChainAndNullableChainUseTailLoop) {
  auto chain = declare("Chain");
  complete(chain, {scalar(), chain});
  EXPECT_EQ(chain.classifyRecordDrop(), RecordDropKind::TailLoop);
  auto nullable = declare("NullableChain");
  auto link =
      RcType::get(&context, nullable, Capability::shared, AtomicKind::normal);
  complete(nullable, {NullableType::get(&context, link), scalar()});
  EXPECT_EQ(nullable.classifyRecordDrop(), RecordDropKind::TailLoop);
}

TEST_F(RecordDropTest, TreeUsesWorklist) {
  auto tree = declare("Tree");
  complete(tree, {tree, tree});
  EXPECT_EQ(tree.classifyRecordDrop(), RecordDropKind::Worklist);
  EXPECT_FALSE(tree.hasObservableCleanup());
}

TEST_F(RecordDropTest, MutualRecursionUsesTailLoop) {
  auto a = declare("A");
  auto b = declare("B");
  complete(a, {b});
  complete(b, {a});
  EXPECT_EQ(a.classifyRecordDrop(), RecordDropKind::TailLoop);
  EXPECT_EQ(b.classifyRecordDrop(), RecordDropKind::TailLoop);
}

TEST_F(RecordDropTest, MutualBranchingUsesWorklist) {
  auto a = declare("A");
  auto b = declare("B");
  complete(a, {b});
  complete(b, {a, a});
  EXPECT_EQ(a.classifyRecordDrop(), RecordDropKind::Worklist);
  EXPECT_EQ(b.classifyRecordDrop(), RecordDropKind::Worklist);
}

TEST_F(RecordDropTest, AlternativesAreNotSimultaneousChildren) {
  auto variant = declare("Variant", RecordKind::variant);
  auto left = declare("Left");
  auto right = declare("Right");
  complete(left, {scalar(), variant}, Capability::value);
  complete(right, {variant, scalar()}, Capability::value);
  complete(variant, {scalar(), left, right});
  EXPECT_EQ(variant.classifyRecordDrop(), RecordDropKind::TailLoop);
}

TEST_F(RecordDropTest, ValueVariantWithBoxedConstructorUsesTailLoop) {
  auto list = declare("List", RecordKind::variant);
  auto cons = declare("Cons");
  complete(list, {scalar(), cons}, Capability::value);
  complete(cons, {scalar(), list});
  EXPECT_EQ(list.classifyRecordDrop(), RecordDropKind::TailLoop);
  EXPECT_EQ(cons.classifyRecordDrop(), RecordDropKind::TailLoop);
}

TEST_F(RecordDropTest, NestedNontrivialSuffixUsesWorklist) {
  auto leaf = declare("Leaf");
  complete(leaf, {scalar()});
  auto suffix = declare("Suffix");
  complete(suffix, {leaf}, Capability::value);
  auto chain = declare("Chain");
  complete(chain, {chain, suffix});
  EXPECT_EQ(chain.classifyRecordDrop(), RecordDropKind::Worklist);
}

TEST_F(RecordDropTest, InlinePayloadCanHideBranching) {
  auto variant = declare("Tree", RecordKind::variant);
  auto payload = declare("Pair");
  complete(payload, {variant, variant}, Capability::value);
  complete(variant, {scalar(), payload});
  EXPECT_EQ(variant.classifyRecordDrop(), RecordDropKind::Worklist);
}

TEST_F(RecordDropTest, AcyclicOwnerOfTreeIsDirect) {
  auto tree = declare("Tree");
  complete(tree, {tree, tree});
  auto owner = declare("Owner");
  complete(owner, {tree});
  EXPECT_EQ(owner.classifyRecordDrop(), RecordDropKind::Direct);
}

TEST_F(RecordDropTest, NonTailDropUsesWorklist) {
  auto leaf = declare("Leaf");
  complete(leaf, {scalar()});
  auto chain = declare("Chain");
  complete(chain, {chain, leaf});
  EXPECT_EQ(chain.classifyRecordDrop(), RecordDropKind::Worklist);
  auto tail = declare("Tail");
  complete(tail, {leaf, tail});
  EXPECT_EQ(tail.classifyRecordDrop(), RecordDropKind::TailLoop);
}

TEST_F(RecordDropTest, TrivialInlineSuffixDoesNotPreventTailLoop) {
  auto value = declare("Value");
  complete(value, {scalar()}, Capability::value);
  auto chain = declare("Chain");
  complete(chain, {chain, value});
  EXPECT_EQ(chain.classifyRecordDrop(), RecordDropKind::TailLoop);
}

TEST_F(RecordDropTest, BorrowedFieldsDoNotAddDropEdges) {
  auto record = declare("Borrowed");
  record.complete({record, record}, {true, true}, Capability::shared, false);
  EXPECT_EQ(record.classifyRecordDrop(), RecordDropKind::Direct);
}

TEST_F(RecordDropTest, IncompleteTypesAreNotCached) {
  auto record = declare("Incomplete");
  EXPECT_EQ(record.classifyRecordDrop(), RecordDropKind::Invalid);
  EXPECT_TRUE(record.hasObservableCleanup());
  complete(record, {record});
  EXPECT_EQ(record.classifyRecordDrop(), RecordDropKind::TailLoop);
  EXPECT_FALSE(record.hasObservableCleanup());
}

TEST_F(RecordDropTest, OpaqueAtomicAndRegionalMembersAreClassified) {
  auto record = declare("ClosureOwner");
  complete(record, {ClosureType::get(&context, {}, scalar())});
  EXPECT_TRUE(record.hasObservableCleanup());
  EXPECT_EQ(record.classifyRecordDrop(), RecordDropKind::Direct);
  auto atomic = declare("Atomic");
  complete(atomic,
           {NullableType::get(&context,
                              RcType::get(&context, atomic, Capability::shared,
                                          AtomicKind::atomic))});
  EXPECT_EQ(atomic.classifyRecordDrop(), RecordDropKind::TailLoop);
  auto regional = declare("Regional");
  complete(regional, {regional}, Capability::regional);
  EXPECT_EQ(regional.classifyRecordDrop(), RecordDropKind::Direct);
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
    EXPECT_EQ(record.classifyRecordDrop(), expected);
  }
}

TEST_F(RecordDropTest, RecursiveCellsAccountForLockCleanup) {
  for (auto kind : {CellKind::plain, CellKind::mutex, CellKind::flatlock,
                    CellKind::rwlock}) {
    auto record = declare(stringifyCellKind(kind));
    auto link =
        RcType::get(&context, record, Capability::shared, AtomicKind::atomic);
    complete(record, {CellType::get(&context, link, kind)});
    EXPECT_EQ(record.classifyRecordDrop(), kind == CellKind::plain
                                               ? RecordDropKind::TailLoop
                                               : RecordDropKind::Worklist);
    EXPECT_EQ(record.hasObservableCleanup(), kind != CellKind::plain);
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
  EXPECT_EQ(record.classifyRecordDrop(), RecordDropKind::Worklist);
  auto owner = declare("ForeignOwner");
  complete(owner, {link});
  EXPECT_EQ(owner.classifyRecordDrop(), RecordDropKind::Direct);
  EXPECT_TRUE(owner.hasObservableCleanup());
}

TEST_F(RecordDropTest, ForeignMemberPositionPreservesTailEligibility) {
  auto foreign = FFIObjectType::get(
      &context, mlir::StringAttr::get(&context, "OrderedForeign"),
      mlir::FlatSymbolRefAttr::get(&context, "cleanup"));
  auto handle = RcType::get(&context, foreign, Capability::shared);
  auto nullable = NullableType::get(&context, handle);
  auto boxed = declare("BoxedForeign");
  complete(boxed, {handle});
  auto inlined = declare("InlineForeign");
  complete(inlined, {boxed}, Capability::value);
  for (auto [idx, suffix] : llvm::enumerate(
           llvm::ArrayRef<mlir::Type>{handle, nullable, inlined})) {
    auto before = declare(("BeforeForeign" + llvm::Twine(idx)).str());
    auto after = declare(("AfterForeign" + llvm::Twine(idx)).str());
    auto beforeLink = NullableType::get(
        &context, RcType::get(&context, before, Capability::shared));
    auto afterLink = NullableType::get(
        &context, RcType::get(&context, after, Capability::shared));
    complete(before, {suffix, beforeLink});
    complete(after, {afterLink, suffix});
    EXPECT_EQ(before.classifyRecordDrop(), RecordDropKind::TailLoop);
    EXPECT_EQ(after.classifyRecordDrop(), RecordDropKind::Worklist);
    EXPECT_TRUE(before.hasObservableCleanup());
    EXPECT_TRUE(after.hasObservableCleanup());
  }
}

TEST_F(RecordDropTest, ScalarBoxesArraysAndCellsAreDirect) {
  auto record = declare("Containers");
  complete(record,
           {NullableType::get(
                &context, RcType::get(&context, scalar(), Capability::shared)),
            ArrayType::get(&context, {2}, scalar()),
            CellType::get(&context, scalar(), CellKind::plain)});
  EXPECT_EQ(record.classifyRecordDrop(), RecordDropKind::Direct);
  EXPECT_FALSE(record.hasObservableCleanup());
}

TEST_F(RecordDropTest, ObservableCleanupPropagatesAcrossMutualRecursion) {
  auto a = declare("ObservableA");
  auto b = declare("ObservableB");
  complete(a, {b});
  complete(b, {a, ClosureType::get(&context, {}, scalar())});
  EXPECT_TRUE(a.hasObservableCleanup());
  EXPECT_TRUE(b.hasObservableCleanup());
}

TEST_F(RecordDropTest, TargetArrayCleanupIsObservable) {
  auto record = declare("DeviceArrayOwner");
  auto target = TargetAttr::get(
      &context, mlir::DenseI32ArrayAttr::get(&context, {0}), {}, {}, {});
  complete(record, {ArrayType::get(&context, {0}, scalar(), target)});
  EXPECT_TRUE(record.hasObservableCleanup());
}

TEST_F(RecordDropTest, IllFormedRecordsHaveNoClassification) {
  EXPECT_EQ(RecordType{}.classifyRecordDrop(), RecordDropKind::Invalid);
  EXPECT_TRUE(RecordType{}.hasObservableCleanup());
  EXPECT_TRUE(RecordType{}.hasRecursiveCleanup());
  auto cycle = declare("InlineCycle");
  complete(cycle, {cycle}, Capability::value);
  EXPECT_EQ(cycle.classifyRecordDrop(), RecordDropKind::Invalid);
  auto incomplete = declare("IncompleteChild");
  auto owner = declare("Owner");
  complete(owner, {incomplete});
  EXPECT_EQ(owner.classifyRecordDrop(), RecordDropKind::Invalid);
  EXPECT_TRUE(owner.hasObservableCleanup());
  complete(incomplete, {incomplete});
  EXPECT_EQ(owner.classifyRecordDrop(), RecordDropKind::Direct);
  EXPECT_EQ(incomplete.classifyRecordDrop(), RecordDropKind::TailLoop);
  EXPECT_FALSE(owner.hasObservableCleanup());
}

TEST_F(RecordDropTest, RecursiveCleanupIncludesOwnedDescendants) {
  auto tree = declare("RecursiveChild");
  complete(tree, {tree, tree});
  auto owner = declare("DirectOwner");
  complete(owner, {tree});
  EXPECT_EQ(owner.classifyRecordDrop(), RecordDropKind::Direct);
  EXPECT_TRUE(owner.hasRecursiveCleanup());
  auto leaf = declare("IncompleteLeaf");
  auto wrapper = declare("LeafWrapper");
  complete(wrapper, {leaf});
  EXPECT_TRUE(wrapper.hasRecursiveCleanup());
  EXPECT_TRUE(wrapper.hasWorklistCleanup());
  complete(leaf, {scalar()});
  auto outer = declare("OuterLeafWrapper");
  complete(outer, {wrapper});
  EXPECT_FALSE(wrapper.hasRecursiveCleanup());
  EXPECT_FALSE(outer.hasRecursiveCleanup());
  EXPECT_FALSE(outer.hasWorklistCleanup());
}

TEST_F(RecordDropTest, WorklistCleanupDistinguishesTailDescendants) {
  auto list = declare("TailChild");
  complete(list, {scalar(), list});
  auto wrapper = declare("TailWrapper");
  complete(wrapper, {list});
  EXPECT_FALSE(list.hasWorklistCleanup());
  EXPECT_FALSE(wrapper.hasWorklistCleanup());
  auto tree = declare("BranchChild");
  complete(tree, {tree, tree});
  auto owner = declare("TailWithBranch");
  complete(owner, {tree, owner});
  EXPECT_EQ(owner.classifyRecordDrop(), RecordDropKind::TailLoop);
  EXPECT_TRUE(owner.hasWorklistCleanup());
  auto outer = declare("BranchWrapper");
  complete(outer, {owner});
  EXPECT_TRUE(outer.hasWorklistCleanup());
}

TEST_F(RecordDropTest, WorklistCleanupRetainsUnsupportedTailEntries) {
  auto regional = declare("RegionalTail");
  complete(regional, {RcType::get(&context, regional, Capability::flex)});
  EXPECT_EQ(regional.classifyRecordDrop(), RecordDropKind::TailLoop);
  EXPECT_TRUE(regional.hasWorklistCleanup());
  auto named = declare("NamedTail");
  auto anonymous =
      RecordType::get(&context, {named}, {false}, RecordKind::compound,
                      Capability::shared, false);
  complete(named, {anonymous});
  EXPECT_EQ(named.classifyRecordDrop(), RecordDropKind::TailLoop);
  EXPECT_TRUE(named.hasWorklistCleanup());
}

TEST_F(RecordDropTest, CachedClassificationIsSharedByTypeHandles) {
  auto record = declare("Cached");
  complete(record, {record});
  EXPECT_EQ(record.classifyRecordDrop(), RecordDropKind::TailLoop);
  auto same = declare("Cached");
  EXPECT_EQ(same, record);
  EXPECT_EQ(same.classifyRecordDrop(), RecordDropKind::TailLoop);
  EXPECT_FALSE(same.hasObservableCleanup());
}

TEST_F(RecordDropTest, SharedAcyclicGraphDoesNotExpandPaths) {
  auto record = declare("Leaf");
  complete(record, {scalar()});
  for (unsigned i = 0; i < 64; ++i) {
    auto parent = declare("Level" + std::to_string(i));
    complete(parent, {record, record});
    record = parent;
  }
  EXPECT_EQ(record.classifyRecordDrop(), RecordDropKind::Direct);
  EXPECT_FALSE(record.hasRecursiveCleanup());
}

#ifdef REUSSIR_ENABLE_RCGMP
// An integer member is a managed leaf released in source order: before the
// recursive link it keeps the tail loop, after it the drop needs a worklist.
TEST_F(RecordDropTest, IntegerMemberPositionPreservesTailEligibility) {
  context.getOrLoadDialect<mlir::rcgmp::RCGMPDialect>();
  auto integer = mlir::rcgmp::MpzType::get(&context, /*atomic=*/false);
  auto leaf = declare("IntegerLeaf");
  complete(leaf, {integer, scalar()});
  EXPECT_FALSE(isTriviallyCopyable(leaf));
  EXPECT_EQ(leaf.classifyRecordDrop(), RecordDropKind::Direct);
  EXPECT_TRUE(leaf.hasObservableCleanup());
  auto before = declare("IntegerBefore");
  complete(before, {integer, before});
  EXPECT_EQ(before.classifyRecordDrop(), RecordDropKind::TailLoop);
  auto after = declare("IntegerAfter");
  complete(after, {after, integer});
  EXPECT_EQ(after.classifyRecordDrop(), RecordDropKind::Worklist);
}
#endif
} // namespace
} // namespace reussir
