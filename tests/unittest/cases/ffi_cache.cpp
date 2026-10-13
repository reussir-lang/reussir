#include "Reussir/RustCompiler/Cache.h"
#include <gtest/gtest.h>
#include <llvm/ADT/SmallString.h>
#include <llvm/Support/FileSystem.h>
#include <memory>
#include <string_view>

// Exercise the native static archive's ABI, ownership and persistent shutdown.
// Rust compiler executables reach these same symbols through the Cargo rlib.
TEST(FfiCacheTest, NativeClientPersistsAcrossClose) {
  llvm::SmallString<128> directory;
  ASSERT_FALSE(
      llvm::sys::fs::createUniqueDirectory("reussir-ffi-cache", directory));
  struct Cleanup {
    llvm::StringRef directory;
    ~Cleanup() { (void)llvm::sys::fs::remove_directories(directory); }
  } cleanup{directory};
  auto close = [](ReussirFfiCache *cache) {
    (void)reussir_ffi_cache_close(cache);
  };
  auto open = [&] {
    return std::unique_ptr<ReussirFfiCache, decltype(close)>(
        reussir_ffi_cache_open(
            reinterpret_cast<const uint8_t *>(directory.data()),
            directory.size(), 0, 0, 0),
        close);
  };
  auto cache = open();
  ASSERT_NE(cache, nullptr);
  constexpr uint8_t key[] = {1, 0, 3};
  constexpr uint8_t value[] = {4, 0, 6};
  ASSERT_EQ(reussir_ffi_cache_insert(cache.get(), key, sizeof(key), value,
                                     sizeof(value)),
            0);
  ASSERT_EQ(reussir_ffi_cache_close(cache.release()), 0);
  cache = open();
  ASSERT_NE(cache, nullptr);
  ReussirFfiCacheBuffer *buffer = nullptr;
  ASSERT_EQ(reussir_ffi_cache_get(cache.get(), key, sizeof(key), &buffer), 1);
  std::unique_ptr<ReussirFfiCacheBuffer,
                  decltype(&reussir_ffi_cache_buffer_free)>
      owned(buffer, reussir_ffi_cache_buffer_free);
  // Returned bytes remain owned by the caller after cache shutdown.
  ASSERT_EQ(reussir_ffi_cache_close(cache.release()), 0);
  EXPECT_EQ(reussir_ffi_cache_buffer_size(buffer), sizeof(value));
  EXPECT_EQ(
      std::string_view(
          reinterpret_cast<const char *>(reussir_ffi_cache_buffer_data(buffer)),
          reussir_ffi_cache_buffer_size(buffer)),
      std::string_view(reinterpret_cast<const char *>(value), sizeof(value)));
}
