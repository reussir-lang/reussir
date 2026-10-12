// RUN: %reussir-opt %s -reussir-token-reuse | %FileCheck %s
// RUN: %reussir-opt %s '-reussir-token-reuse=reuse-across-call=true' | %FileCheck %s

// A token no allocation reuses is freed once, where it is made: in the
// unique branch of its decrement. That held for a decrement's own token;
// a token escaped from a decrement nested in another (a member released
// with its owner) was instead freed after the owner, and under
// `reuse-across-call`, where tokens stay available across calls, at every
// exit after it: quadratic code in the nesting of matches. It is now freed
// at the end of the owner's unique branch, as is a token that branch takes
// from the guard that skips a tagged immediate (special pointer tags).

!inner = !reussir.record<compound "pair" {i64, i64}>
!rcInner = !reussir.rc<!inner>
!outer = !reussir.record<compound "node" {!inner, i64}>
!rcOuter = !reussir.rc<!outer>
!tk = !reussir.token<align: 8, size: 24>

module @test attributes {dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>, #dlti.dl_entry<i8, dense<8> : vector<2xi64>>, #dlti.dl_entry<!llvm.ptr, dense<64> : vector<4xi64>>, #dlti.dl_entry<"dlti.endianness", "little">>, llvm.data_layout = "e-m:e-i64:64-n8:16:32:64-S128"} {
    func.func private @work()

    // CHECK-LABEL: func.func @sink_escaped
    // CHECK: scf.if
    // CHECK: %[[MT:.+]] = scf.if
    // CHECK: } {reussir.expanded_decrement}
    // CHECK: %[[TOK:.+]] = reussir.rc.reinterpret
    // CHECK-DAG: reussir.token.free(%[[TOK]] : !reussir.token<align : 8, size : 24>)
    // CHECK-DAG: reussir.token.free(%[[MT]] : !reussir.nullable<!reussir.token<align : 8, size : 24>>)
    // CHECK: scf.yield
    // CHECK: } {reussir.expanded_decrement}
    // CHECK-NOT: reussir.token.free
    // CHECK: call @work
    // CHECK-NOT: reussir.token.free
    // CHECK: return
    func.func @sink_escaped(%o: !rcOuter, %flag: i1) {
        %prev = reussir.rc.fetch (%o : !rcOuter) : index
        %c1 = arith.constant 1 : index
        %isOne = arith.cmpi eq, %prev, %c1 : index
        %unique = reussir.expect (%isOne : i1, true) : i1
        %t = scf.if %unique -> (!reussir.nullable<!tk>) {
            %ref = reussir.rc.borrow (%o : !rcOuter) : !reussir.ref<!outer>
            %slot = reussir.ref.project (%ref : !reussir.ref<!outer>) [0] : !reussir.ref<!rcInner>
            %m = reussir.ref.load (%slot : !reussir.ref<!rcInner>) : !rcInner
            %mprev = reussir.rc.fetch (%m : !rcInner) : index
            %misOne = arith.cmpi eq, %mprev, %c1 : index
            %munique = reussir.expect (%misOne : i1, true) : i1
            %mt = scf.if %munique -> (!reussir.nullable<!tk>) {
                %mtok = reussir.rc.reinterpret (%m : !rcInner) : !tk
                %mnn = reussir.nullable.create (%mtok : !tk) : !reussir.nullable<!tk>
                scf.yield %mnn : !reussir.nullable<!tk>
            } else {
                %mdec = arith.subi %mprev, %c1 : index
                reussir.rc.set (%m : !rcInner, %mdec : index)
                %mnull = reussir.nullable.create : !reussir.nullable<!tk>
                scf.yield %mnull : !reussir.nullable<!tk>
            } {reussir.expanded_decrement}
            %tok = reussir.rc.reinterpret (%o : !rcOuter) : !tk
            %nn = reussir.nullable.create (%tok : !tk) : !reussir.nullable<!tk>
            scf.yield %nn : !reussir.nullable<!tk>
        } else {
            %dec = arith.subi %prev, %c1 : index
            reussir.rc.set (%o : !rcOuter, %dec : index)
            %null = reussir.nullable.create : !reussir.nullable<!tk>
            scf.yield %null : !reussir.nullable<!tk>
        } {reussir.expanded_decrement}
        func.call @work() : () -> ()
        scf.if %flag {
            func.call @work() : () -> ()
        }
        return
    }

    // CHECK-LABEL: func.func @sink_guarded
    // CHECK: %[[G:.+]] = scf.if %arg1
    // CHECK: reussir.rc.reinterpret
    // CHECK: scf.yield
    // CHECK-NEXT: }
    // CHECK-NEXT: reussir.token.free(%[[G]] : !reussir.nullable<!reussir.token<align : 8, size : 24>>)
    // CHECK-NEXT: scf.yield %[[G]]
    // CHECK: } {reussir.expanded_decrement}
    // CHECK-NOT: reussir.token.free
    // CHECK: call @work
    // CHECK-NOT: reussir.token.free
    // CHECK: return
    func.func @sink_guarded(%o: !rcOuter, %immediate: i1) {
        %prev = reussir.rc.fetch (%o : !rcOuter) : index
        %c1 = arith.constant 1 : index
        %isOne = arith.cmpi eq, %prev, %c1 : index
        %unique = reussir.expect (%isOne : i1, true) : i1
        %t = scf.if %unique -> (!reussir.nullable<!tk>) {
            %g = scf.if %immediate -> (!reussir.nullable<!tk>) {
                %none = reussir.nullable.create : !reussir.nullable<!tk>
                scf.yield %none : !reussir.nullable<!tk>
            } else {
                %tok = reussir.rc.reinterpret (%o : !rcOuter) : !tk
                %nn = reussir.nullable.create (%tok : !tk) : !reussir.nullable<!tk>
                scf.yield %nn : !reussir.nullable<!tk>
            }
            scf.yield %g : !reussir.nullable<!tk>
        } else {
            %dec = arith.subi %prev, %c1 : index
            reussir.rc.set (%o : !rcOuter, %dec : index)
            %null = reussir.nullable.create : !reussir.nullable<!tk>
            scf.yield %null : !reussir.nullable<!tk>
        } {reussir.expanded_decrement}
        func.call @work() : () -> ()
        return
    }

    // Reuse on one successor prevents sinking into the producer: the other
    // successor must still free the token, including an initially empty else.
    // CHECK-LABEL: func.func @keep_reused
    // CHECK: %[[T:.+]] = scf.if
    // CHECK-NOT: reussir.token.free
    // CHECK: } {reussir.expanded_decrement}
    // CHECK: scf.if %arg2
    // CHECK-NOT: reussir.token.free
    // CHECK: reussir.token.ensure(%[[T]]
    // CHECK: } else {
    // CHECK-NEXT: reussir.token.free(%[[T]]
    // CHECK-NEXT: }
    // CHECK-NOT: reussir.token.free
    // CHECK: return
    func.func @keep_reused(%o: !rcOuter, %immediate: i1, %take: i1, %v: !inner) {
        %prev = reussir.rc.fetch (%o : !rcOuter) : index
        %c1 = arith.constant 1 : index
        %isOne = arith.cmpi eq, %prev, %c1 : index
        %unique = reussir.expect (%isOne : i1, true) : i1
        %t = scf.if %unique -> (!reussir.nullable<!tk>) {
            %g = scf.if %immediate -> (!reussir.nullable<!tk>) {
                %none = reussir.nullable.create : !reussir.nullable<!tk>
                scf.yield %none : !reussir.nullable<!tk>
            } else {
                %tok = reussir.rc.reinterpret (%o : !rcOuter) : !tk
                %nn = reussir.nullable.create (%tok : !tk) : !reussir.nullable<!tk>
                scf.yield %nn : !reussir.nullable<!tk>
            }
            scf.yield %g : !reussir.nullable<!tk>
        } else {
            %dec = arith.subi %prev, %c1 : index
            reussir.rc.set (%o : !rcOuter, %dec : index)
            %null = reussir.nullable.create : !reussir.nullable<!tk>
            scf.yield %null : !reussir.nullable<!tk>
        } {reussir.expanded_decrement}
        scf.if %take {
            %fresh = reussir.token.alloc : !tk
            %new = reussir.rc.create value(%v : !inner) token(%fresh : !tk) : !rcInner
            func.call @consume(%new) : (!rcInner) -> ()
        }
        return
    }
    func.func private @consume(!rcInner)
}
