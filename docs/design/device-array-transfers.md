# Device array allocation and host transfers

The host representation remains a scalar or an ordinary array `[T; dims]`.
A `DevArray<T, P>` owns device buffers and a reference-counted descriptor;
`T` carries the full scalar or array shape and `P` selects placement.
Omitting `P` selects `DefaultPlacement` (logical device zero).

## API

`core::intrinsic::pjrt` provides:

| Operation | Arguments | Result |
| --- | --- | --- |
| `upload<P>(host)` | A host scalar or array; optional placement type | Owned `DevArray` with the same dtype and shape |
| `download(device)` | A `DevArray<T, P>` | A scalar for rank zero, otherwise a fresh host array |
| `alloc<DevArray<T, P>>(extents..., value)` | One `u64` extent for each `?`, in dimension order, then the fill value | Initialized device array |
| `dim(device, axis)` | Device array and `u64` axis | Logical `u64` extent |

Allocation uses the established extents-first host-array convention:

```reussir
import core::intrinsic::pjrt;

fn example(n:u64) -> [f32; ?, 4] {
    let device = pjrt::alloc<DevArray<[f32; ?, 4]>>(n, 1.5);
    pjrt::download(device)
}
```

Static and rank-zero allocations take only the fill value. Zero extents are
valid. Negative/non-`u64` extents are rejected by type checking; values that
cannot fit the target index width, signed dimensions, or allocation size fail
at runtime before allocation. `dim` checks the axis against the rank.

The core declaration anchor supplies ordinary generic methods
`device.download()`, `device.dim(axis)`, and
`DevArray<T, P>::from_host(host)`. Their bodies call the same free intrinsics;
member lookup does not attach special behavior to method names. Generic
`DevArray<T, P>` captures the whole scalar/shape specification in `T`.

## Ownership and execution

Transfers borrow their input until completion. Upload returns independent
owned device storage; download returns fresh owned host storage. Reusing an
input after transfer and independently modifying downloaded arrays follows
the ordinary reference-counting and copy-on-write rules.

These operations are synchronous. Allocation initializes host staging memory,
uploads it, and releases the staging allocation. It is not a device-native
fill, and there is no uninitialized device-allocation API. Transfers do not
return a `Future`; kernel execution and kernel async operations have separate
contracts. Host transfer intrinsics are rejected inside kernel functions.

The lifetime rule follows the pinned
[PJRT host-buffer semantics](https://github.com/openxla/xla/blob/fc840b37e26bd36c99f32e295324a1700dc12ecd/xla/pjrt/c/pjrt_c_api.h):
host storage stays immutable until the upload completion event. The runtime
waits before returning or releasing staging storage. Download likewise waits
for completion before exposing its result.

Supported dtypes are bool, signed/unsigned 8/16/32/64-bit integers, f16/f32/f64,
and bf16. Host integer storage is signless in MLIR; transfers preserve the
device dtype's signedness without accepting different widths or numeric
reinterpretations. Strings, records, pointers, and managed element types
cannot be device payloads.

## Placement and shape

Named placement markers use the existing `#[placement(...)]` metadata.
Ordered device IDs and sharding metadata pass unchanged to the native transfer
operations. Tiled and replicated arrays support uneven logical dimensions,
including empty arrays and tiles wholly outside the logical shape. The runtime
uses padded physical tiles and copies only their clipped logical intersections,
as described in [IFRT host transfers](ifrt-build.md#host-transfers-for-sharded-arrays).

Dynamic logical extents live in the managed device descriptor, independently
of padded physical shard sizes. The `reussir.array.device_extent` operation
reads this metadata; download uses it to allocate the correct host shape.
The caller never reconstructs logical extents by multiplying tile sizes.

Unspecified sharding cannot determine a host distribution. Unreduced shard
values require an explicit reduction before materialization because placement
alone does not specify a reduction operator. These existing native diagnostics
remain intact. Backend failures abort, matching the current PJRT runtime API.

Dynamic host arrays now lower through the same checked strided-header storage
used by native arrays. Existing splat/tabulate, get/set, fold, and dim operations
accept runtime extents with their existing element-type rules (managed elements
use splat/get/set; tabulate/fold require scalar elements). This does not add reshape, slicing, or resizing APIs.
