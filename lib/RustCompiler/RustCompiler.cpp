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
/// This file implements helpers for locating and invoking the Rust compiler.
///
//===----------------------------------------------------------------------===//

#include "Reussir/RustCompiler.h"
#include <array>
#include <cstdint>
#include <cstdlib>
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/SmallString.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/StringExtras.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/BLAKE3.h>
#include <llvm/Support/CachePruning.h>
#include <llvm/Support/Caching.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/FileUtilities.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/Path.h>
#include <llvm/Support/Program.h>
#include <llvm/TargetParser/Host.h>
#include <map>
#include <memory>
#include <mutex>
#include <optional>
#include <set>
#include <system_error>
#if defined(__unix__) || defined(__APPLE__)
#include <sys/stat.h>
#endif

#ifdef _WIN32
#define EXEC_SUFFIX ".exe"
#else
#define EXEC_SUFFIX ""
#endif

#ifdef _WIN32
#define LIB_NAME "reussir_rt.dll"
#elif defined(__APPLE__)
#define LIB_NAME "libreussir_rt.dylib"
#else
#define LIB_NAME "libreussir_rt.so"
#endif

namespace reussir {
namespace {
constexpr std::array<llvm::StringRef, 9> RUSTC_HINTS = {
    "rustc" EXEC_SUFFIX,
    "build/bin/rustc" EXEC_SUFFIX,
    "bin/rustc" EXEC_SUFFIX,
    "../bin/rustc" EXEC_SUFFIX,
    "../../bin/rustc" EXEC_SUFFIX,
    "../../../bin/rustc" EXEC_SUFFIX,
    "/usr/bin/rustc" EXEC_SUFFIX,
    "/usr/local/bin/rustc" EXEC_SUFFIX,
    "/opt/reussir/bin/rustc" EXEC_SUFFIX,
};
constexpr std::array<llvm::StringRef, 14> RUSTC_DEPS_HINTS = {
    "lib/" LIB_NAME,
    "../lib/" LIB_NAME,
    "../../lib/" LIB_NAME,
    "../../../lib/" LIB_NAME,
    "bin/" LIB_NAME,
    "../bin/" LIB_NAME,
    "../../bin/" LIB_NAME,
    "../../../bin/" LIB_NAME,
    "/usr/lib/" LIB_NAME,
    "/usr/local/lib/" LIB_NAME,
    "/opt/reussir/lib/" LIB_NAME,
};

//===----------------------------------------------------------------------===//
// Texture bitcode cache
//===----------------------------------------------------------------------===//
//
// Every polymorphic-FFI texture is compiled by its own rustc run, one after
// another, and programs built against the same packages compile the same
// textures again and again: with large packages, these runs can take most of
// a build. When the REUSSIR_FFI_CACHE_DIR environment variable names a
// directory, llvm::localCache keeps the bitcode for later compilations.
// Unset (the default), nothing changes. LLVM periodically prunes old entries.
// The directory must be trusted: entries are checked for damage, not
// authenticated, and whoever can write there chooses the code rrc links.
//
// The key is a BLAKE3 digest of the explicitly identified inputs: the source,
// rustc's options, rustc itself (its resolved path, the file's content, which
// tells wrapper scripts apart, and its `-vV` report, which names the
// toolchain a rustup proxy runs), the host CPU (for `-C target-cpu=native`)
// and the content of every library in the package directories (the crates
// `-L` lets rustc load). Files or environment variables read at compile time
// by a texture or by the macros of the crates it uses (`include!`, `env!`,
// procedural macros), file contents named by options, and dependencies injected
// by wrappers outside those directories are not tracked. Enable this cache only
// when those inputs are fixed or the caller manages their invalidation. The
// selected toolchain must also remain fixed throughout the compiler process.
//
// The files that part of the key was computed from are stamped (device,
// inode, size, modification time and, on POSIX systems, status-change time)
// when they are hashed, and checked again before each lookup and before each
// store: a library replaced while rrc runs (another build writing the package
// directories) gets a new digest, and a texture compiled while it changed is
// not stored, so that no entry is filed under a key that does not describe
// what rustc read. Elsewhere, a library rewritten in place with the same size
// and its old modification time restored goes unseen.
//
// LLVM owns lookup, access-time updates and atomic publication. Each file
// contains a BLAKE3 digest followed by bitcode. Invalid entries are discarded;
// cache failures fall back to compilation.

constexpr llvm::StringLiteral CACHE_FORMAT = "reussir-polyffi-llvm-cache-v1";
using Digest = llvm::BLAKE3Result<>;

/// Hashes a length-prefixed field, so that consecutive fields cannot run
/// into each other.
void hashField(llvm::BLAKE3 &hasher, llvm::StringRef field) {
  uint64_t size = field.size();
  hasher.update(llvm::ArrayRef<uint8_t>(
      reinterpret_cast<const uint8_t *>(&size), sizeof(size)));
  hasher.update(field);
}

/// A file or directory as it was when the toolchain digest read it.
struct FileStamp {
  std::string path;
  llvm::sys::fs::UniqueID id;
  uint64_t size = 0;
  llvm::sys::TimePoint<> modified;
  // The status-change time (seconds, nanoseconds), which no one can set
  // back: it catches a rewrite that restores the modification time.
  std::array<int64_t, 2> changed = {};

  bool operator==(const FileStamp &) const = default;
};

std::optional<FileStamp> stampFile(llvm::StringRef path) {
  llvm::sys::fs::file_status status;
  if (llvm::sys::fs::status(path, status))
    return std::nullopt;
  FileStamp stamp{path.str(), status.getUniqueID(), status.getSize(),
                  status.getLastModificationTime()};
#if defined(__unix__) || defined(__APPLE__)
  struct stat st;
  if (::stat(stamp.path.c_str(), &st) != 0)
    return std::nullopt;
#if defined(__APPLE__)
  stamp.changed = {st.st_ctimespec.tv_sec, st.st_ctimespec.tv_nsec};
#else
  stamp.changed = {st.st_ctim.tv_sec, st.st_ctim.tv_nsec};
#endif
#endif
  return stamp;
}

/// The files of `dir` rustc can load crates from, sorted.
std::error_code listLibraries(llvm::StringRef dir,
                              llvm::SmallVectorImpl<std::string> &libraries) {
  std::error_code code;
  for (llvm::sys::fs::directory_iterator file(dir, code), end;
       file != end && !code; file.increment(code)) {
    llvm::StringRef extension = llvm::sys::path::extension(file->path());
    if (extension == ".rlib" || extension == ".rmeta" || extension == ".so" ||
        extension == ".dylib" || extension == ".dll")
      libraries.push_back(file->path());
  }
  llvm::sort(libraries);
  return code;
}

/// A package directory as the toolchain digest listed it.
struct DirStamp {
  FileStamp stamp;
  llvm::SmallVector<std::string> libraries;
};

/// Guards the toolchains computed so far, including their directory stamps.
std::mutex toolchainMutex;

/// The part of the key that does not depend on the texture, and the files
/// it was computed from.
struct Toolchain {
  Digest digest;
  /// rustc and every library.
  llvm::SmallVector<FileStamp> files;
  /// The package directories. A directory's stamp changes whenever an entry
  /// is added or removed, also one rustc does not read (rrc's temporary
  /// files when it runs there, a cache directory made there): a changed
  /// stamp counts only if the libraries listed there changed, and is
  /// refreshed otherwise.
  mutable llvm::SmallVector<DirStamp> dirs;

  /// Whether every file is still the one that was hashed. Call with
  /// `toolchainMutex` held.
  bool unchanged() const {
    if (!llvm::all_of(files, [](const FileStamp &file) {
          return stampFile(file.path) == file;
        }))
      return false;
    for (DirStamp &dir : dirs) {
      std::optional<FileStamp> now = stampFile(dir.stamp.path);
      if (!now)
        return false;
      if (*now == dir.stamp)
        continue;
      llvm::SmallVector<std::string> libraries;
      if (listLibraries(dir.stamp.path, libraries) ||
          libraries != dir.libraries)
        return false;
      dir.stamp = std::move(*now);
    }
    return true;
  }
};

/// rustc's `-vV` report; none when rustc cannot be run or fails.
std::optional<std::string> rustcVersion(llvm::StringRef rustcPath) {
  llvm::SmallString<128> outPath;
  if (llvm::sys::fs::createTemporaryFile("reussir_rustc_version", "txt",
                                         outPath))
    return std::nullopt;
  llvm::FileRemover outRemover(outPath);
  llvm::StringRef args[] = {"rustc", "-vV"};
  std::optional<llvm::StringRef> redirects[] = {
      llvm::StringRef(), llvm::StringRef(outPath), llvm::StringRef()};
  if (llvm::sys::ExecuteAndWait(rustcPath, args, std::nullopt, redirects) != 0)
    return std::nullopt;
  auto out = llvm::MemoryBuffer::getFile(outPath);
  if (!out)
    return std::nullopt;
  return (*out)->getBuffer().str();
}

/// The directory of a `-L` argument, which may start with a search kind.
llvm::StringRef searchDirectory(llvm::StringRef path) {
  for (llvm::StringRef kind :
       {"dependency=", "crate=", "native=", "framework=", "all="})
    if (path.consume_front(kind))
      break;
  return path;
}

/// Hashes rustc, the host CPU and the libraries in `depsPaths`, stamping
/// every file it reads; the reason when something cannot be read or run.
llvm::Expected<Toolchain>
computeToolchain(llvm::StringRef rustcPath,
                 llvm::ArrayRef<std::string> depsPaths) {
  auto failure = [](const llvm::Twine &reason) {
    return llvm::createStringError(reason);
  };
  Toolchain toolchain;
  llvm::BLAKE3 hasher;
  // Stamps `path`, then hashes its name and content. Read into owned storage:
  // a concurrent in-place truncate must not SIGBUS through an mmap before the
  // post-hash stamp check can reject the changing input.
  auto hashFile = [&](llvm::StringRef path,
                      llvm::StringRef name) -> llvm::Error {
    std::optional<FileStamp> stamp = stampFile(path);
    auto content = llvm::MemoryBuffer::getFile(path, /*IsText=*/false,
                                               /*RequiresNullTerminator=*/false,
                                               /*IsVolatile=*/true);
    if (!stamp || !content)
      return failure("cannot read " + path);
    toolchain.files.push_back(std::move(*stamp));
    hashField(hasher, name);
    hashField(hasher, (*content)->getBuffer());
    return llvm::Error::success();
  };
  if (llvm::Error error = hashFile(rustcPath, rustcPath))
    return std::move(error);
  std::optional<std::string> version = rustcVersion(rustcPath);
  if (!version)
    return failure("`" + rustcPath + " -vV` failed");
  hashField(hasher, *version);
  hashField(hasher, llvm::sys::getHostCPUName());
  llvm::SmallVector<std::string> features;
  for (const auto &feature : llvm::sys::getHostCPUFeatures())
    features.push_back((feature.second ? "+" : "-") + feature.first().str());
  llvm::sort(features);
  for (const std::string &feature : features)
    hashField(hasher, feature);
  for (const std::string &depsPath : depsPaths) {
    llvm::StringRef dir = searchDirectory(depsPath);
    hashField(hasher, depsPath);
    std::optional<FileStamp> dirStamp = stampFile(dir);
    if (!dirStamp)
      return failure("cannot read the directory " + dir);
    DirStamp &listed = toolchain.dirs.emplace_back();
    listed.stamp = std::move(*dirStamp);
    if (std::error_code code = listLibraries(dir, listed.libraries))
      return failure("cannot list the directory " + dir + ": " +
                     code.message());
    for (const std::string &library : listed.libraries)
      if (llvm::Error error =
              hashFile(library, llvm::sys::path::filename(library)))
        return std::move(error);
  }
  if (!toolchain.unchanged())
    return failure("a file changed while it was hashed");
  toolchain.digest = hasher.final();
  return toolchain;
}

/// The toolchain part of the key for `rustcPath` and `depsPaths`, computed
/// once per process (one rrc run), since hashing the libraries reads them all
/// (about 220 MB for lean2rr's directories, most of it the toolchain's
/// standard library), and again when one of its files has changed. Null when
/// caching is off for the rest of the run: something could not be read or
/// run, or files changed while they were hashed (said once, on stderr).
std::shared_ptr<const Toolchain>
toolchainFor(llvm::StringRef rustcPath, llvm::ArrayRef<std::string> depsPaths) {
  static std::map<std::string, std::shared_ptr<const Toolchain>> computed;
  // Relative -L paths and rustup directory overrides depend on the working
  // directory. Keep process-local snapshots separate when an embedding changes
  // directories between compilation requests.
  llvm::SmallString<128> cwd;
  if (llvm::sys::fs::current_path(cwd))
    return nullptr;
  std::string id = cwd.str().str();
  id += '\0';
  id += rustcPath.str();
  for (const std::string &depsPath : depsPaths)
    id += '\0' + depsPath;
  std::lock_guard<std::mutex> lock(toolchainMutex);
  auto [it, inserted] = computed.try_emplace(id);
  if (!inserted && (!it->second || it->second->unchanged()))
    return it->second;
  llvm::Expected<Toolchain> toolchain = computeToolchain(rustcPath, depsPaths);
  if (!toolchain) {
    llvm::errs() << "warning: REUSSIR_FFI_CACHE_DIR: no polymorphic-FFI "
                    "caching in this run: "
                 << llvm::toString(toolchain.takeError()) << "\n";
    it->second = nullptr;
    return nullptr;
  }
  it->second = std::make_shared<const Toolchain>(std::move(*toolchain));
  return it->second;
}

/// Check pruning once per directory per compiler process. LLVM's timestamp
/// additionally limits directory scans across processes to its default
/// interval.
void pruneCacheOnce(llvm::StringRef directory) {
  static std::mutex mutex;
  static std::set<std::string> checked;
  std::lock_guard<std::mutex> lock(mutex);
  if (!checked.insert(directory.str()).second)
    return;
  llvm::CachePruningPolicy policy;
  policy.MaxSizeBytes = 1024ULL * 1024 * 1024;
  if (auto pruned = llvm::pruneCache(directory, policy); !pruned)
    llvm::consumeError(pruned.takeError());
}

/// A texture key and the snapshot it describes. The callback state lives only
/// for this compilation request; the cache retains no in-process bitcode.
struct CacheEntry {
  std::string key;
  llvm::FileCache cache;
  llvm::AddStreamFn addStream;
  std::shared_ptr<std::unique_ptr<llvm::MemoryBuffer>> buffer;
  std::shared_ptr<const Toolchain> toolchain;

  bool current() const {
    std::lock_guard<std::mutex> lock(toolchainMutex);
    return toolchain->unchanged();
  }
};

/// The cache entry for a texture compiled by `rustcPath` with `options`
/// (rustc's arguments but the source and output files); none when the cache
/// is off.
std::optional<CacheEntry> cacheEntryFor(llvm::StringRef sourceCode,
                                        llvm::ArrayRef<llvm::StringRef> options,
                                        llvm::StringRef rustcPath,
                                        llvm::ArrayRef<std::string> depsPaths) {
  const char *cacheDir = std::getenv("REUSSIR_FFI_CACHE_DIR");
  if (!cacheDir || !*cacheDir)
    return std::nullopt;
  llvm::SmallString<128> directory(cacheDir);
  if (llvm::sys::fs::make_absolute(directory))
    return std::nullopt;
  llvm::sys::path::remove_dots(directory, /*remove_dot_dot=*/true);
  pruneCacheOnce(directory);
  auto buffer = std::make_shared<std::unique_ptr<llvm::MemoryBuffer>>();
  auto cache =
      llvm::localCache("Reussir polyffi", "reussir-polyffi", directory,
                       [buffer](unsigned, const llvm::Twine &,
                                std::unique_ptr<llvm::MemoryBuffer> result) {
                         *buffer = std::move(result);
                       });
  if (!cache) {
    llvm::consumeError(cache.takeError());
    return std::nullopt;
  }
  std::shared_ptr<const Toolchain> toolchain =
      toolchainFor(rustcPath, depsPaths);
  if (!toolchain)
    return std::nullopt;
  llvm::BLAKE3 hasher;
  hashField(hasher, CACHE_FORMAT);
  hasher.update(toolchain->digest);
  for (llvm::StringRef option : options)
    hashField(hasher, option);
  hashField(hasher, sourceCode);
  return CacheEntry{llvm::toHex(hasher.final(), /*LowerCase=*/true),
                    std::move(*cache),
                    {},
                    std::move(buffer),
                    std::move(toolchain)};
}

std::unique_ptr<llvm::MemoryBuffer> readCacheEntry(CacheEntry &entry) {
  // Retry once after removing a damaged hit. A concurrent writer may already
  // have replaced it; losing a valid cache entry is harmless, and retries must
  // remain bounded when another process keeps changing it.
  for (unsigned attempt = 0; attempt != 2; ++attempt) {
    auto stream = entry.cache(0, entry.key, "polyffi");
    if (!stream) {
      llvm::consumeError(stream.takeError());
      return nullptr;
    }
    entry.addStream = std::move(*stream);
    if (entry.addStream)
      return nullptr;
    auto buffer = std::move(*entry.buffer);
    if (!buffer)
      return nullptr;
    llvm::StringRef content = buffer->getBuffer();
    if (content.size() >= sizeof(Digest)) {
      llvm::StringRef bitcode = content.drop_front(sizeof(Digest));
      Digest digest = llvm::BLAKE3::hash(llvm::arrayRefFromStringRef(bitcode));
      if (content.take_front(sizeof(Digest)) == llvm::toStringRef(digest) &&
          llvm::isBitcode(bitcode.bytes_begin(), bitcode.bytes_end()))
        return llvm::MemoryBuffer::getMemBufferCopy(bitcode, "polyffi-cache");
    }
    std::string path = buffer->getBufferIdentifier().str();
    buffer.reset();
    if (llvm::sys::fs::remove(path))
      return nullptr;
  }
  return nullptr;
}

void writeCacheEntry(const CacheEntry &entry, llvm::StringRef bitcode) {
  if (!entry.addStream)
    return;
  // Create the stream only after successful compilation and revalidation.
  // LLVM requires every created stream to be committed exactly once.
  auto stream = entry.addStream(0, "polyffi");
  if (!stream) {
    llvm::consumeError(stream.takeError());
    return;
  }
  // localCache's output stream is a raw_fd_ostream. Clear write errors before
  // its destruction so cache I/O failures cannot terminate the compiler.
  auto &out = static_cast<llvm::raw_fd_ostream &>(*(*stream)->OS);
  out << llvm::toStringRef(
             llvm::BLAKE3::hash(llvm::arrayRefFromStringRef(bitcode)))
      << bitcode;
  out.flush();
  bool failed = out.has_error();
  out.clear_error();
  llvm::consumeError((*stream)->commit());
  if (failed)
    (void)llvm::sys::fs::remove((*stream)->ObjectPathName);
}
} // namespace

llvm::StringRef findRustCompiler(llvm::StringRef preferred) {
  // an explicit path from the driver wins over the environment and the probe
  if (!preferred.empty())
    return preferred;
  // first check if REUSSIR_RUSTC is set
  if (const char *env_p = std::getenv("REUSSIR_RUSTC"))
    return env_p;
  // A bare `rustc` is the normal installation shape. ExecuteAndWait requires
  // a resolved program path, so recognize PATH here before trying the
  // project-relative fallback locations below.
  if (llvm::sys::findProgramByName(RUSTC_HINTS.front()))
    return RUSTC_HINTS.front();
  // locate rustc in known paths
  for (const auto &path : llvm::drop_begin(RUSTC_HINTS)) {
    if (llvm::sys::fs::exists(path))
      return path;
  }

  return "";
}

llvm::SmallVector<std::string>
findRustCompilerDeps(llvm::ArrayRef<llvm::StringRef> preferred) {
  // explicit directories from the driver win over the environment and the
  // probe
  if (!preferred.empty()) {
    llvm::SmallVector<std::string> dirs;
    for (llvm::StringRef dir : preferred)
      if (!dir.empty())
        dirs.push_back(dir.str());
    if (!dirs.empty())
      return dirs;
  }
  // first check if REUSSIR_RUSTC_DEPS is set; it may list several directories
  // separated by the platform's environment path separator
  if (const char *env_p = std::getenv("REUSSIR_RUSTC_DEPS")) {
    llvm::SmallVector<std::string> dirs;
    llvm::SmallVector<llvm::StringRef> parts;
    llvm::StringRef(env_p).split(parts, llvm::sys::EnvPathSeparator,
                                 /*MaxSplit=*/-1, /*KeepEmpty=*/false);
    for (llvm::StringRef dir : parts)
      dirs.push_back(dir.str());
    if (!dirs.empty())
      return dirs;
  }
  for (const auto &path : RUSTC_DEPS_HINTS) {
    if (llvm::sys::fs::exists(path))
      return {llvm::sys::path::parent_path(path).str()};
  }
  return {};
}

std::unique_ptr<llvm::MemoryBuffer> compileRustSourceToBitcode(
    llvm::LLVMContext &context, llvm::StringRef sourceCode,
    llvm::ArrayRef<llvm::StringRef> additionalArgs, llvm::StringRef rustcPath,
    llvm::ArrayRef<llvm::StringRef> rustcDepsDirs) {
  rustcPath = findRustCompiler(rustcPath);
  // ExecuteAndWait does not search PATH: its Program parameter is expected to
  // have come from findProgramByName. Resolve explicit/env bare names as well
  // as the default discovery result before spawning.
  auto resolvedRustc = llvm::sys::findProgramByName(rustcPath);
  if (!resolvedRustc) {
    llvm::errs() << "Could not find rustc executable `" << rustcPath << "`\n";
    return nullptr;
  }
  std::string rustcExecutable = *resolvedRustc;
  rustcPath = rustcExecutable;
  llvm::SmallVector<std::string> rustcDepsPaths =
      findRustCompilerDeps(rustcDepsDirs);
  if (rustcPath.empty() || rustcDepsPaths.empty()) {
    llvm::SmallString<16> cwd;
    auto code = llvm::sys::fs::current_path(cwd);
    if (code) {
      cwd = "<unknown>";
    }
    llvm::errs() << "Could not find rustc or its dependencies, current "
                    "working directory: "
                 << cwd << "\n";
    return nullptr;
  }
  // rustc's options, without the source and output files: the cache key
  // leaves out these temporary files, whose random names reach the output
  // only as the crate's name, the module's source file name and the file
  // names of panic locations, random anyway.
  llvm::SmallVector<llvm::StringRef, 24> options = {
      "-A", "warnings", "--crate-type", "cdylib", "--emit=llvm-bc"};
  for (const std::string &depsPath : rustcDepsPaths) {
    options.push_back("-L");
    options.push_back(depsPath);
  }
  for (auto arg : additionalArgs)
    options.push_back(arg);
  std::optional<CacheEntry> cacheEntry =
      cacheEntryFor(sourceCode, options, rustcPath, rustcDepsPaths);
  if (cacheEntry) {
    if (std::unique_ptr<llvm::MemoryBuffer> cached =
            readCacheEntry(*cacheEntry)) {
      // Recheck after reading: a package library may have changed since the
      // snapshot used to form the key was checked.
      if (cacheEntry->current())
        return cached;
    }
  }
  // Create a temporary file for the source code
  llvm::SmallString<32> srcFilePath;
  llvm::SmallString<32> resultBitcodeFilePath;
  std::error_code srcFileCode = llvm::sys::fs::createUniqueFile(
      "reussir_rust_module_%%%%%%.rs", srcFilePath);
  if (srcFileCode) {
    llvm::errs() << "Could not create temporary Rust source file\n";
    return nullptr;
  }
  llvm::FileRemover srcFileRemover(srcFilePath);
  std::error_code resultBitcodeFileCode = llvm::sys::fs::createUniqueFile(
      "reussir_rust_module_%%%%%%.bc", resultBitcodeFilePath);
  if (resultBitcodeFileCode) {
    llvm::errs() << "Could not create temporary Rust bitcode file\n";
    return nullptr;
  }
  llvm::FileRemover resultBitcodeFileRemover(resultBitcodeFilePath);
  {
    int fd = -1;
    std::error_code code = llvm::sys::fs::openFileForWrite(srcFilePath, fd);
    if (code) {
      llvm::errs() << "Could not open temporary file for writing: "
                   << code.message() << "\n";
      return nullptr;
    }
    llvm::raw_fd_ostream srcStream(fd, /*shouldClose=*/true);
    srcStream << sourceCode;
    srcStream.flush();
  }
  // Prepare rustc command
  llvm::SmallVector<llvm::StringRef, 24> args = {"rustc", srcFilePath, "-o",
                                                 resultBitcodeFilePath};
  llvm::append_range(args, options);
  // Execute rustc
  std::string executionError;
  int code = llvm::sys::ExecuteAndWait(rustcPath, args, std::nullopt, {},
                                       /*SecondsToWait=*/0, /*MemoryLimit=*/0,
                                       &executionError);
  if (code != 0) {
    llvm::errs() << "Rust compilation failed with exit code " << code << "\n";
    if (!executionError.empty())
      llvm::errs() << "Failed to execute rustc: " << executionError << "\n";
    llvm::errs() << "Full command: " << rustcPath << " ";
    // args[0] is argv[0], not a second command-line argument.
    llvm::interleave(llvm::drop_begin(args), llvm::errs(), " ");
    llvm::errs() << "\n";
    return nullptr;
  }
  // Load the bitcode file into a buffer
  std::unique_ptr<llvm::MemoryBuffer> buffer;
  {
    auto bufferOrErr = llvm::MemoryBuffer::getFile(resultBitcodeFilePath);
    if (!bufferOrErr) {
      llvm::errs() << "Failed to read bitcode file: "
                   << bufferOrErr.getError().message() << "\n";
      return {};
    }
#ifdef _WIN32
    buffer = llvm::MemoryBuffer::getMemBufferCopy((*bufferOrErr)->getBuffer());
#else
    buffer = std::move(*bufferOrErr);
#endif
  }
  // Not stored if a file the key was computed from changed meanwhile: rustc
  // may have read the new one.
  if (cacheEntry && cacheEntry->current())
    writeCacheEntry(*cacheEntry, buffer->getBuffer());
  return buffer;
}

std::unique_ptr<llvm::Module>
compileRustSource(llvm::LLVMContext &context, llvm::StringRef sourceCode,
                  llvm::ArrayRef<llvm::StringRef> additionalArgs,
                  llvm::StringRef rustcPath,
                  llvm::ArrayRef<llvm::StringRef> rustcDepsDirs) {
  std::unique_ptr<llvm::MemoryBuffer> bitcode = compileRustSourceToBitcode(
      context, sourceCode, additionalArgs, rustcPath, rustcDepsDirs);
  if (!bitcode)
    return nullptr;
  auto moduleOrErr =
      llvm::parseBitcodeFile(bitcode->getMemBufferRef(), context);
  if (!moduleOrErr) {
    llvm::errs() << "Failed to parse bitcode file: "
                 << llvm::toString(moduleOrErr.takeError()) << "\n";
    return nullptr;
  }
  return std::move(*moduleOrErr);
}
} // namespace reussir
