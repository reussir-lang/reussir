!R = !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0]>>>
module {
  memref.global "private" constant @input : memref<4xf32> = dense<[1.0, 2.0, 3.0, 4.0]>
  func.func private @_RC11launch_keep(!R) -> !R
  func.func private @_RC12launch_reuse(!R) -> !R
  func.func private @_RC12launch_async(!R) -> !R
  func.func private @check(index, f32)
  func.func @examples() {
    %src = memref.get_global @input : memref<4xf32>
    %dst = memref.alloca() : memref<4xf32>
    %input = reussir.array.to_device %src : memref<4xf32> -> !R
    reussir.rc.inc(%input : !R)
    %kept = func.call @_RC11launch_keep(%input) : (!R) -> !R
    reussir.rc.inc(%input : !R)
    %reused = func.call @_RC12launch_reuse(%input) : (!R) -> !R
    %async = func.call @_RC12launch_async(%input) : (!R) -> !R
    %zero = arith.constant 0 : index
    %four = arith.constant 4 : index
    %one = arith.constant 1 : index
    reussir.array.to_host %kept into %dst : !R, memref<4xf32>
    scf.for %i = %zero to %four step %one {
      %value = memref.load %dst[%i] : memref<4xf32>
      func.call @check(%i, %value) : (index, f32) -> ()
    }
    reussir.array.to_host %reused into %dst : !R, memref<4xf32>
    scf.for %i = %zero to %four step %one {
      %value = memref.load %dst[%i] : memref<4xf32>
      func.call @check(%i, %value) : (index, f32) -> ()
    }
    reussir.array.to_host %async into %dst : !R, memref<4xf32>
    scf.for %i = %zero to %four step %one {
      %value = memref.load %dst[%i] : memref<4xf32>
      func.call @check(%i, %value) : (index, f32) -> ()
    }
    reussir.rc.dec(%kept : !R)
    reussir.rc.dec(%reused : !R)
    reussir.rc.dec(%async : !R)
    return
  }
}
