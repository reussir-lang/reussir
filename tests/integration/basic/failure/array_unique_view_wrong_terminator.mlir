// RUN: %reussir-opt %s -verify-diagnostics

!arr4 = !reussir.array<4 x i8>
!rc_arr4 = !reussir.rc<!arr4>

module {
  func.func @bad_body_terminator(%xs: !rc_arr4) -> !rc_arr4 {
    // expected-error @+1 {{'reussir.array.with_unique_view' op body region must terminate with reussir.scf.yield}}
    %res = reussir.array.with_unique_view (%xs : !rc_arr4) -> !rc_arr4 {
      ^bb0(%view: !reussir.view<mutable, 4 x i8>):
        cf.br ^bb0(%view : !reussir.view<mutable, 4 x i8>)
    }
    return %res : !rc_arr4
  }
}
