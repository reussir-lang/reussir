#s = #ifrt.sharding_param<2 to [0] on 2>
!Tiled = !reussir.rc<!reussir.array<5 x i32, #reussir.target<devices = [1, 0], sharding = #s>>>
!Unsigned = !reussir.rc<!reussir.array<ui64, #reussir.target<devices = [0]>>>
!Bool = !reussir.rc<!reussir.array<i1, #reussir.target<devices = [0]>>>
module {
  memref.global "private" constant @values : memref<5xi32> = dense<[3, 7, 11, 19, 23]>
  memref.global "private" constant @high : memref<ui64> = dense<18446744073709551615>
  memref.global "private" constant @low : memref<ui64> = dense<9223372036854775808>
  memref.global "private" constant @yes : memref<i1> = dense<true>
  memref.global "private" constant @no : memref<i1> = dense<false>
  func.func private @_RC13sharded_shape(!Tiled) -> !Tiled
  func.func private @_RC12scalar_shape(!Unsigned, !Unsigned, !Bool) -> !Unsigned
  func.func private @check_shard(index, i32)
  func.func private @check_unsigned(index, i64)
  func.func @shape_examples() {
    %src = memref.get_global @values : memref<5xi32>
    %dst = memref.alloca() : memref<5xi32>
    %input = reussir.array.to_device %src : memref<5xi32> -> !Tiled
    reussir.rc.inc(%input : !Tiled)
    %result = func.call @_RC13sharded_shape(%input) : (!Tiled) -> !Tiled
    %zero = arith.constant 0 : index
    %five = arith.constant 5 : index
    %one = arith.constant 1 : index
    reussir.array.to_host %result into %dst : !Tiled, memref<5xi32>
    scf.for %i = %zero to %five step %one {
      %value = memref.load %dst[%i] : memref<5xi32>
      func.call @check_shard(%i, %value) : (index, i32) -> ()
    }
    // The caller's retained buffer must remain valid after both source launches.
    reussir.array.to_host %input into %dst : !Tiled, memref<5xi32>
    scf.for %i = %zero to %five step %one {
      %value = memref.load %dst[%i] : memref<5xi32>
      func.call @check_shard(%i, %value) : (index, i32) -> ()
    }
    reussir.rc.dec(%input : !Tiled)
    reussir.rc.dec(%result : !Tiled)

    %high_src = memref.get_global @high : memref<ui64>
    %low_src = memref.get_global @low : memref<ui64>
    %yes_src = memref.get_global @yes : memref<i1>
    %no_src = memref.get_global @no : memref<i1>
    %high_dev = reussir.array.to_device %high_src : memref<ui64> -> !Unsigned
    %low_dev = reussir.array.to_device %low_src : memref<ui64> -> !Unsigned
    %yes_dev = reussir.array.to_device %yes_src : memref<i1> -> !Bool
    %no_dev = reussir.array.to_device %no_src : memref<i1> -> !Bool
    reussir.rc.inc(%high_dev : !Unsigned)
    reussir.rc.inc(%low_dev : !Unsigned)
    %true_result = func.call @_RC12scalar_shape(%high_dev, %low_dev, %yes_dev) : (!Unsigned, !Unsigned, !Bool) -> !Unsigned
    %false_result = func.call @_RC12scalar_shape(%high_dev, %low_dev, %no_dev) : (!Unsigned, !Unsigned, !Bool) -> !Unsigned
    %scalar_dst = memref.alloca() : memref<ui64>
    reussir.array.to_host %true_result into %scalar_dst : !Unsigned, memref<ui64>
    %true_bits = memref.extract_aligned_pointer_as_index %scalar_dst : memref<ui64> -> index
    %high_bits = arith.constant -1 : i64
    func.call @check_unsigned(%true_bits, %high_bits) : (index, i64) -> ()
    reussir.array.to_host %false_result into %scalar_dst : !Unsigned, memref<ui64>
    %false_bits = memref.extract_aligned_pointer_as_index %scalar_dst : memref<ui64> -> index
    %low_bits = arith.constant -9223372036854775808 : i64
    func.call @check_unsigned(%false_bits, %low_bits) : (index, i64) -> ()
    reussir.rc.dec(%true_result : !Unsigned)
    reussir.rc.dec(%false_result : !Unsigned)
    return
  }
}
