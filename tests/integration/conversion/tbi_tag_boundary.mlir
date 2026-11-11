// RUN: %python %S/Inputs/tbi_tag_boundary.py 255 | %reussir-opt --reussir-special-pointer-tag --reussir-token-instantiation --reussir-rc-create-fusion --convert-to-llvm | %reussir-translate --mlir-to-llvmir | %FileCheck %s --check-prefix=FITS
// RUN: %python %S/Inputs/tbi_tag_boundary.py 256 | %reussir-opt --reussir-special-pointer-tag --reussir-token-instantiation --reussir-rc-create-fusion --convert-to-llvm | %reussir-translate --mlir-to-llvmir | %FileCheck %s --check-prefix=HEADER
// RUN: %python %S/Inputs/tbi_tag_boundary.py 255 | %reussir-opt --reussir-token-instantiation --reussir-rc-create-fusion --convert-to-llvm | %reussir-translate --mlir-to-llvmir | %opt -S -passes=instcombine | %FileCheck %s --check-prefix=LAST

// LAST-LABEL: define ptr @last(
// LAST: or i64 {{.*}}, -72057594037927936
// LAST: ret ptr

// FITS: REUSSIR_TAG_DUMMY
// FITS-LABEL: define i64 @tag(
// FITS-NOT: load
// FITS-NOT: br
// FITS: lshr i64 {{.*}}, 56
// FITS: sub i64 {{.*}}, 1
// FITS: ret i64
// FITS-LABEL: define ptr @nil(
// FITS-NOT: call
// FITS: ret ptr

// HEADER-NOT: REUSSIR_TAG_DUMMY
// HEADER-LABEL: define i64 @tag(
// HEADER-NOT: lshr
// HEADER: load i32
// HEADER: ret i64
// HEADER-LABEL: define ptr @nil(
// HEADER: call ptr @__reussir_allocate
// HEADER-NOT: ptrtoint
// HEADER: ret ptr
// HEADER-LABEL: define ptr @last(
// HEADER: call ptr @__reussir_allocate
// HEADER-NOT: ptrtoint
// HEADER: ret ptr
