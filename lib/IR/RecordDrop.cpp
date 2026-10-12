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
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/SCCIterator.h>
#include <memory>

namespace reussir::detail {
struct DropNode {
  mlir::Type type;
  llvm::SmallVector<DropNode *> children;
  llvm::SmallVector<bool> owned;
  bool hasDrop = false;
  bool alternatives = false;
  bool repeated = false;
  bool cleanupAfterChild = false;
};
} // namespace reussir::detail

namespace llvm {
template <> struct GraphTraits<reussir::detail::DropNode *> {
  using NodeRef = reussir::detail::DropNode *;
  using ChildIteratorType = SmallVectorImpl<NodeRef>::iterator;
  static NodeRef getEntryNode(NodeRef node) { return node; }
  static ChildIteratorType child_begin(NodeRef node) {
    return node->children.begin();
  }
  static ChildIteratorType child_end(NodeRef node) {
    return node->children.end();
  }
};
} // namespace llvm

namespace reussir {
std::optional<RecordDropKind> classifyRecordDrop(RecordType record) {
  using detail::DropNode;
  if (!record)
    return std::nullopt;
  llvm::DenseMap<mlir::Type, DropNode *> nodes;
  llvm::SmallVector<std::unique_ptr<DropNode>> storage;
  auto intern = [&](mlir::Type type) {
    auto [it, inserted] = nodes.try_emplace(type);
    if (inserted) {
      storage.push_back(std::make_unique<DropNode>());
      it->second = storage.back().get();
      it->second->type = type;
    }
    return it->second;
  };
  DropNode *root = intern(record);
  for (size_t i = 0; i < storage.size(); ++i) {
    DropNode *node = storage[i].get();
    if (!node->type)
      return std::nullopt;
    auto addChild = [&](mlir::Type type, bool owned = false) {
      node->children.push_back(intern(type));
      node->owned.push_back(owned);
    };
    if (auto record = llvm::dyn_cast<RecordType>(node->type)) {
      if (!record.getComplete() ||
          record.getMembers().size() != record.getMemberIsField().size())
        return std::nullopt;
      node->alternatives = record.isVariant();
      for (auto [member, field] :
           llvm::zip(record.getMembers(), record.getMemberIsField())) {
        if (!member)
          return std::nullopt;
        if (!field)
          addChild(getProjectedType(member, false, Capability::shared));
      }
    } else if (auto nullable = llvm::dyn_cast<NullableType>(node->type)) {
      addChild(nullable.getPtrTy());
    } else if (auto rc = llvm::dyn_cast<RcType>(node->type)) {
      if (rc.getCapability() != Capability::rigid) {
        node->hasDrop = true;
        addChild(rc.getElementType(), true);
      }
    } else if (auto array = llvm::dyn_cast<ArrayType>(node->type)) {
      if (array.hasTargetAttr()) {
        node->hasDrop = true;
      } else if (!llvm::is_contained(array.getShape(), 0)) {
        node->repeated = llvm::any_of(
            array.getShape(), [](int64_t extent) { return extent != 1; });
        addChild(array.getElementType());
      }
    } else if (auto cell = llvm::dyn_cast<CellType>(node->type)) {
      node->cleanupAfterChild =
          cell.getMutex() || cell.getFlatlock() || cell.getRwlock();
      addChild(cell.getElementType());
    } else {
      node->hasDrop = !isTriviallyCopyable(node->type);
    }
  }

  for (auto it = llvm::scc_begin(root); !it.isAtEnd(); ++it) {
    const auto &component = *it;
    llvm::DenseSet<DropNode *> members(component.begin(), component.end());
    bool hasDrop = false;
    bool ownedCycle = false;
    bool branching = false;
    bool tail = true;
    for (DropNode *node : component) {
      hasDrop |= node->hasDrop;
      unsigned recursiveChildren = 0;
      bool afterRecursion = false;
      for (auto [child, owned] : llvm::zip(node->children, node->owned)) {
        bool recursive = members.contains(child);
        hasDrop |= owned || child->hasDrop;
        ownedCycle |= recursive && owned;
        if (!node->alternatives) {
          if (afterRecursion && (recursive || owned || child->hasDrop))
            tail = false;
          afterRecursion |= recursive;
        }
        recursiveChildren += recursive;
      }
      branching |= (!node->alternatives && recursiveChildren > 1) ||
                   (node->repeated && recursiveChildren != 0);
      tail &= !(node->cleanupAfterChild && recursiveChildren != 0);
    }
    for (DropNode *node : component)
      node->hasDrop = hasDrop;
    if (it.hasCycle() && !ownedCycle)
      return std::nullopt;
    if (!members.contains(root))
      continue;
    if (!it.hasCycle())
      return RecordDropKind::Direct;
    if (branching || !tail)
      return RecordDropKind::Worklist;
    return RecordDropKind::TailLoop;
  }
  llvm_unreachable("root must belong to an SCC");
}
} // namespace reussir
