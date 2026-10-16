# Whole-image Mandelbrot on PJRT

[`example_kernel_mandelbrot_ascii.rr`](../../tests/integration/frontend/example_kernel_mandelbrot_ascii.rr)
reads rows, columns, and an iteration limit. Reussir computes the complete image
in one dynamic kernel launch, downloads it once, and renders ASCII or terminal
truecolor. PolyFFI is used only for console I/O. There is no host tiling.

Rows and columns use `pjrt::upload_shape(i64)`. Their exact host values let the
runtime resolve dynamic iota and fill shapes using upstream StableHLO passes.
The iteration limit, pixel scale, and viewport height use ordinary uploads and
remain runtime data. Shapes and explicit shape values participate in caching.
The input contract accepts positive values up to 2147483647; the image must fit
available memory. This example uses device zero and does not add support for
partitioned dynamic outputs without a logical-extent contract.

## Build

From an OpenXLA-enabled Nix development shell on the machine that will run it:

```sh
export RUSTC_WRAPPER=sccache CARGO_BUILD_JOBS=4
cmake --preset nix-dev -B build -DREUSSIR_ENABLE_OPENXLA=ON \
  -DCMAKE_BUILD_TYPE=Release -DREUSSIR_IFRT_JOBS=4 \
  -DCMAKE_C_COMPILER_LAUNCHER=sccache -DCMAKE_CXX_COMPILER_LAUNCHER=sccache
cmake --build build -j 4 --target rrc ReussirStablehloSpecializer
CARGO_TARGET_DIR="$PWD/build/target-pjrt" \
REUSSIR_PJRT_PATH="$PWD/build/_deps/xla-src" \
REUSSIR_STABLEHLO_LIB_DIR="$PWD/build/lib" \
  cargo build --locked --release -p reussir-rt --features pjrt
build/bin/rrc tests/integration/frontend/example_kernel_mandelbrot_ascii.rr \
  --emit executable --runtime-linkage static --relocation-mode pic \
  --polyffi-libdir "$PWD/build/target-pjrt/release/deps" \
  -O aggressive -o /tmp/reussir-mandelbrot
```

Use the PJRT-enabled runtime's dependency directory for PolyFFI as well as the
final executable. Mixing it with a separately built plain runtime causes an
ambiguous Rust crate selection. The sys crate bundles its optional native
archives, so the final link does not need the compiler's native library paths.

## Verified Spark CUDA setup

Validated on NVIDIA GB10 (SM 12.1a), driver 595.71.05, CUDA toolkit/runtime 13.3,
with the ARM64 `pjrt-cuda_linux-arm64.tar.gz` artifact from
[zml/pjrt-artifacts, manual-2026-09-08T13-23-00Z](https://github.com/zml/pjrt-artifacts/releases/tag/manual-2026-09-08T13-23-00Z),
cuDNN 9.27.0.42, and NCCL 2.32.3. CPU validation uses the CPU artifact from the
same release.

Keep downloaded plugins and dependencies outside the source tree. For example:

```sh
task_dir=/tmp/reussir-mandelbrot-deps
mkdir -p "$task_dir/plugins/cuda" "$task_dir/cuda-driver"
gh release download manual-2026-09-08T13-23-00Z --repo zml/pjrt-artifacts \
  --pattern pjrt-cuda_linux-arm64.tar.gz --dir "$task_dir/plugins"
tar -xf "$task_dir/plugins/pjrt-cuda_linux-arm64.tar.gz" -C "$task_dir/plugins/cuda"
# Use the system Python installer outside the Nix shell for these binary wheels.
/usr/bin/python3 -m pip install --no-deps --target "$task_dir/cuda-deps" \
  nvidia-cudnn-cu13==9.27.0.42 nvidia-nccl-cu13==2.32.3
for driver in /lib/aarch64-linux-gnu/libcuda.so* /lib/aarch64-linux-gnu/libnvidia*.so*; do
  ln -sf "$driver" "$task_dir/cuda-driver/"
done
```

The driver-only directory matters with Nix: adding the entire Ubuntu system
library directory to `LD_LIBRARY_PATH` can load an incompatible system libc.
With `task_dir` set to the prepared directory, run inside `nix develop`:

```sh
export REUSSIR_PJRT_PLUGIN="$task_dir/plugins/cuda/libpjrt_cuda.so"
export LD_LIBRARY_PATH="$task_dir/cuda-driver:$task_dir/cuda-deps/nvidia/cudnn/lib:$task_dir/cuda-deps/nvidia/nccl/lib:/usr/local/cuda/lib64:${LD_LIBRARY_PATH:-}"
export XLA_FLAGS=--xla_gpu_cuda_data_dir=/usr/local/cuda
export TF_FORCE_UNIFIED_MEMORY=1
printf '3 5 32\n' | /tmp/reussir-mandelbrot
python3 tests/integration/frontend/Inputs/mandelbrot_ascii_check.py /tmp/reussir-mandelbrot
```

The first command produces:

```text
:rssr
@@@@2
:rssr
```

The independent Python checker compares complete 17×129 and 19×131 images at
low iteration limits against scalar complex-number orbits. Low limits avoid
fractal-boundary sensitivity to CPU/GPU floating-point contraction. The lit test
also checks the 1×1 image and invalid input. Both CPU and CUDA runs pass these
checks. On the tested Spark, the plugin emits an NVML `Not Supported` diagnostic
while still executing the kernel successfully.

For optional lit execution, set `REUSSIR_PJRT_TEST_RUNTIME` to
`$PWD/build/target-pjrt/release/libreussir_rt.so` and keep the backend environment
above. Use `-j 1` for CUDA to avoid concurrent whole-device allocator contexts.
