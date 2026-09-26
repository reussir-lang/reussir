# Raw PjRt bindings

Bindgen generates the upstream PjRt C API from the pinned header in `vendor/`.
Building requires libclang. The crate does not link XLA or provide a runtime
wrapper; plugin loading, ABI compatibility and ownership belong to the caller.
`libloading` and `paste` are test-only dependencies.

The CPU smoke test embeds a handwritten StableHLO checkerboard kernel and calls
the C API directly to compile, execute, download and check its pixels:

```sh
REUSSIR_PJRT_PLUGIN=/absolute/path/to/libpjrt_cpu.so \
  cargo test --locked -p reussir-pjrt-sys -- --include-ignored
```

Without a plugin, the test is reported as ignored. It was exercised with the
Linux x86-64 CPU plugin from [ZML's 2026-09-08 release](https://github.com/zml/pjrt-artifacts/releases/tag/manual-2026-09-08T13-23-00Z)
(C API 0.114). The kernel is also checked by the StableHLO lit suite.

`tests/fixtures/compile_options.pb` is generated from its adjacent textproto
with upstream protoc. To regenerate using the XLA revision in `vendor/README.md`:

```sh
protoc -I "$XLA_SOURCE" --encode=xla.CompileOptionsProto \
  xla/pjrt/proto/compile_options.proto \
  < crates/reussir-pjrt-sys/tests/fixtures/compile_options.textproto \
  > crates/reussir-pjrt-sys/tests/fixtures/compile_options.pb
```

Existing general Rust bindings show small public adoption. On 2026-09-26,
[`pjrt`](https://crates.io/crates/pjrt) had 2,382 total downloads, 63 recent
downloads and no published reverse dependencies; [`ryft-pjrt`](https://crates.io/crates/ryft-pjrt)
had 243 total, 76 recent and two reverse dependencies within Ryft. Those figures
do not establish private production usage.
