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
/// This file defines `reussir::Generator`: `std::generator` where the standard
/// library provides it, and a single-pass coroutine range otherwise.
///
//===----------------------------------------------------------------------===//

#pragma once

#ifndef REUSSIR_SUPPORT_GENERATOR_H
#define REUSSIR_SUPPORT_GENERATOR_H

#include <version>

#ifdef __cpp_lib_generator

#include <generator>

namespace reussir {
template <typename T> using Generator = std::generator<T>;
} // namespace reussir

#else

#include <coroutine>
#include <cstddef>
#include <exception>
#include <iterator>
#include <memory>
#include <utility>

namespace reussir {
// Covers what `std::generator<T>` offers for a value type `T`: `co_yield` of
// a `T` and one pass over the results with a range-based `for`.
template <typename T> class Generator {
public:
  struct promise_type {
    const T *current = nullptr;

    Generator get_return_object() {
      return Generator(
          std::coroutine_handle<promise_type>::from_promise(*this));
    }
    std::suspend_always initial_suspend() noexcept { return {}; }
    std::suspend_always final_suspend() noexcept { return {}; }
    // The yielded value outlives the suspension, so its address stays valid
    // until the coroutine is resumed.
    std::suspend_always yield_value(const T &value) noexcept {
      current = std::addressof(value);
      return {};
    }
    void return_void() noexcept {}
    void await_transform() = delete;
    [[noreturn]] void unhandled_exception() noexcept { std::terminate(); }
  };

  class iterator {
  public:
    using value_type = T;
    using difference_type = std::ptrdiff_t;

    explicit iterator(std::coroutine_handle<promise_type> handle)
        : handle(handle) {}

    const T &operator*() const { return *handle.promise().current; }
    iterator &operator++() {
      handle.resume();
      return *this;
    }
    void operator++(int) { ++*this; }
    bool operator==(std::default_sentinel_t) const { return handle.done(); }

  private:
    std::coroutine_handle<promise_type> handle;
  };

  Generator(Generator &&other) noexcept
      : handle(std::exchange(other.handle, {})) {}
  Generator &operator=(Generator &&other) noexcept {
    std::swap(handle, other.handle);
    return *this;
  }
  ~Generator() {
    if (handle)
      handle.destroy();
  }

  iterator begin() {
    handle.resume();
    return iterator(handle);
  }
  std::default_sentinel_t end() const noexcept { return {}; }

private:
  explicit Generator(std::coroutine_handle<promise_type> handle)
      : handle(handle) {}

  std::coroutine_handle<promise_type> handle;
};
} // namespace reussir

#endif // __cpp_lib_generator

#endif // REUSSIR_SUPPORT_GENERATOR_H
