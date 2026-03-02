// RUN: %reussir-opt %s --reussir-invariant-group-analysis | %FileCheck %s

// Type aliases
!inner = !reussir.record<compound "Inner" [value] {i64, i64}>
!with_field_ = !reussir.record<compound "WithField" incomplete>
!with_field = !reussir.record<compound "WithField" [regional] {i64, [field] !with_field_}>
!nullable_field_flex = !reussir.nullable<!reussir.rc<!with_field_ flex>>

module {

  // Test 1: Basic rc.borrow -> project -> load (should be marked)
  // CHECK-LABEL: func.func @basic_borrow_project_load
  func.func @basic_borrow_project_load(%rc: !reussir.rc<!inner>) -> i64 {
    %ref = reussir.rc.borrow (%rc : !reussir.rc<!inner>) : !reussir.ref<!inner shared>
    %field_ref = reussir.ref.project (%ref : !reussir.ref<!inner shared>) [0] : !reussir.ref<i64 shared>
    // CHECK: reussir.ref.load{{.*}}invariant_group
    %val = reussir.ref.load (%field_ref : !reussir.ref<i64 shared>) : i64
    return %val : i64
  }

  // Test 2: rc.borrow direct load (should be marked)
  // CHECK-LABEL: func.func @direct_borrow_load
  func.func @direct_borrow_load(%rc: !reussir.rc<i64>) -> i64 {
    %ref = reussir.rc.borrow (%rc : !reussir.rc<i64>) : !reussir.ref<i64 shared>
    // CHECK: reussir.ref.load{{.*}}invariant_group
    %val = reussir.ref.load (%ref : !reussir.ref<i64 shared>) : i64
    return %val : i64
  }

  // Test 3: Chained projections from borrow (should be marked)
  // CHECK-LABEL: func.func @chained_projections
  func.func @chained_projections(
    %rc: !reussir.rc<!reussir.record<compound "Outer" [value] {!inner}>>
  ) -> i64 {
    %ref = reussir.rc.borrow (%rc : !reussir.rc<!reussir.record<compound "Outer" [value] {!inner}>>) : !reussir.ref<!reussir.record<compound "Outer" [value] {!inner}> shared>
    %inner_ref = reussir.ref.project (%ref : !reussir.ref<!reussir.record<compound "Outer" [value] {!inner}> shared>) [0] : !reussir.ref<!inner shared>
    %field_ref = reussir.ref.project (%inner_ref : !reussir.ref<!inner shared>) [0] : !reussir.ref<i64 shared>
    // CHECK: reussir.ref.load{{.*}}invariant_group
    %val = reussir.ref.load (%field_ref : !reussir.ref<i64 shared>) : i64
    return %val : i64
  }

  // Test 4: Flex + field projection (should NOT be marked - tests soundness)
  // CHECK-LABEL: func.func @flex_field_projection
  func.func @flex_field_projection(%rc: !reussir.rc<!with_field flex>) -> !nullable_field_flex {
    %ref = reussir.rc.borrow (%rc : !reussir.rc<!with_field flex>) : !reussir.ref<!with_field flex>
    %field_ref = reussir.ref.project (%ref : !reussir.ref<!with_field flex>) [1] : !reussir.ref<!nullable_field_flex field>
    // CHECK-NOT: invariant_group
    // CHECK: reussir.ref.load
    // CHECK-NOT: invariant_group
    %val = reussir.ref.load (%field_ref : !reussir.ref<!nullable_field_flex field>) : !nullable_field_flex
    return %val : !nullable_field_flex
  }

  // Test 5: Non-flex (shared) + non-field projection (should be marked)
  // CHECK-LABEL: func.func @shared_non_field_projection
  func.func @shared_non_field_projection(%rc: !reussir.rc<!inner>) -> i64 {
    %ref = reussir.rc.borrow (%rc : !reussir.rc<!inner>) : !reussir.ref<!inner shared>
    %field_ref = reussir.ref.project (%ref : !reussir.ref<!inner shared>) [1] : !reussir.ref<i64 shared>
    // CHECK: reussir.ref.load{{.*}}invariant_group
    %val = reussir.ref.load (%field_ref : !reussir.ref<i64 shared>) : i64
    return %val : i64
  }

  // Test 6: Plain function arg -> load (should NOT be marked)
  // CHECK-LABEL: func.func @plain_func_arg
  func.func @plain_func_arg(%ref: !reussir.ref<i64>) -> i64 {
    // CHECK-NOT: invariant_group
    // CHECK: reussir.ref.load
    // CHECK-NOT: invariant_group
    %val = reussir.ref.load (%ref : !reussir.ref<i64>) : i64
    return %val : i64
  }

  // Test 7: Flex + non-field projection (should be marked - field bit is false)
  // CHECK-LABEL: func.func @flex_non_field_projection
  func.func @flex_non_field_projection(%rc: !reussir.rc<!with_field flex>) -> i64 {
    %ref = reussir.rc.borrow (%rc : !reussir.rc<!with_field flex>) : !reussir.ref<!with_field flex>
    %field_ref = reussir.ref.project (%ref : !reussir.ref<!with_field flex>) [0] : !reussir.ref<i64 flex>
    // CHECK: reussir.ref.load{{.*}}invariant_group
    %val = reussir.ref.load (%field_ref : !reussir.ref<i64 flex>) : i64
    return %val : i64
  }

  // Test 8: Load from ref.spilled (should NOT be marked)
  // CHECK-LABEL: func.func @spilled_load
  func.func @spilled_load(%val: !inner) -> i64 {
    %spilled = reussir.ref.spilled (%val : !inner) : !reussir.ref<!inner>
    %field_ref = reussir.ref.project (%spilled : !reussir.ref<!inner>) [0] : !reussir.ref<i64>
    // CHECK-NOT: invariant_group
    // CHECK: reussir.ref.load
    // CHECK-NOT: invariant_group
    %result = reussir.ref.load (%field_ref : !reussir.ref<i64>) : i64
    return %result : i64
  }
}
