#pragma once

#include <functional>
#ifndef REUSSIR_SUPPORT_IMMER_H
#define REUSSIR_SUPPORT_IMMER_H

#include <immer/flex_vector.hpp>
#include <immer/heap/heap_policy.hpp>
#include <immer/map.hpp>
#include <immer/memory_policy.hpp>
#include <immer/set.hpp>
#include <llvm/ADT/Hashing.h>

namespace reussir {
using UnsyncImmerPolicy =
    immer::memory_policy<immer::unsafe_free_list_heap_policy<immer::cpp_heap>,
                         immer::unsafe_refcount_policy, immer::no_lock_policy>;
template <typename T>
using UnsyncFlexVector = immer::flex_vector<T, UnsyncImmerPolicy>;

template <typename T>
using UnsyncSet =
    immer::set<T, std::hash<T>, std::equal_to<T>, UnsyncImmerPolicy>;

template <typename K, typename V>
using UnsyncMap =
    immer::map<K, V, std::hash<K>, std::equal_to<K>, UnsyncImmerPolicy>;
} // namespace reussir

#endif // REUSSIR_SUPPORT_IMMER_H
