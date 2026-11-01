// RUN: %reussir-opt %s --verify-each

module {
  func.func private @first()
  func.func private @second(i64) -> i32
  func.func @declared_uses(%x: !reussir.rc<i64>) {
    reussir.rc.dec(%x : !reussir.rc<i64>) cleanup(@first, @second)
    return
  }
}
