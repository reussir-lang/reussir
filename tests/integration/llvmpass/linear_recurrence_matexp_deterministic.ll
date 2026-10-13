; RUN: %reussir-llvm-opt --linear-recurrence-pipeline=O2 %S/linear_recurrence_order6_e2e.ll -o %t.0.ll
; RUN: for i in 1 2 3 4 5; do %reussir-llvm-opt --linear-recurrence-pipeline=O2 %S/linear_recurrence_order6_e2e.ll -o %t.$i.ll && cmp %t.0.ll %t.$i.ll || exit 1; done
; RUN: %reussir-llvm-opt --passes='function(loop-simplify,reussir-linear-recurrence-matexp)' %S/linear_recurrence_matexp.ll -o %t.fib0.ll
; RUN: for i in 1 2 3 4 5 6 7; do %reussir-llvm-opt --passes='function(loop-simplify,reussir-linear-recurrence-matexp)' %S/linear_recurrence_matexp.ll -o %t.fib$i.ll && cmp %t.fib0.ll %t.fib$i.ll || exit 1; done

; The matrix exponentiation's output must not depend on heap addresses. The
; order of the recurrence state (the rows and columns of the companion matrix,
; and so the emitted code) followed a DenseMap keyed by PHINode pointers:
; repeated runs on the same input printed different, equivalent modules (ten
; distinct ones in twelve runs of the order-6 recurrence). The state now
; follows a deterministic traversal, discovering each expression's PHIs in
; loop header order.
