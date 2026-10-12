; UNSUPPORTED: windows
; RUN: %opt -passes='coro-early,cgscc(coro-split),coro-cleanup' -verify-each -S %s -o %t.ll
; RUN: %llc %t.ll -O0 -relocation-model=pic -filetype=obj -o %t.o
; RUN: %cc -std=c11 -Wall -Wextra -Werror -I%S/../../../include %S/Inputs/dtor_executor.c %t.o -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs -o %t.exe
; RUN: %t.exe
; RUN: %cc -x c++ -std=c++17 -Wall -Wextra -Werror -I%S/../../../include %S/Inputs/dtor_executor.c -x none %t.o -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs -o %t.cpp.exe
; RUN: %t.cpp.exe
; RUN: %opt -passes='default<O2>' -verify-each -S %s -o %t.opt.ll
; RUN: %llc %t.opt.ll -relocation-model=pic -filetype=obj -o %t.opt.o
; RUN: %cc -I%S/../../../include %S/Inputs/dtor_executor.c %t.opt.o -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs -o %t.opt.exe
; RUN: %t.opt.exe

declare token @llvm.coro.id.retcon(i32, i32, ptr, ptr, ptr, ptr)
declare ptr @llvm.coro.begin(token, ptr)
declare ptr @llvm.coro.prepare.retcon(ptr)
declare i1 @llvm.coro.suspend.retcon.i1(...)
declare void @llvm.coro.end(ptr, i1, token)
declare {ptr, ptr} @resume_prototype(ptr, i1 zeroext)
declare ptr @malloc(i64)
declare void @free(ptr)
declare void @observe(i64)

define {ptr, ptr} @start_a(ptr noalias %buffer, ptr %object) presplitcoroutine sanitize_address {
entry:
  %task = alloca {ptr, ptr}, align 8
  %id = call token @llvm.coro.id.retcon(i32 32, i32 8, ptr %buffer,
      ptr @resume_prototype, ptr @malloc, ptr @free)
  %hdl = call ptr @llvm.coro.begin(token %id, ptr null)
  %n = ptrtoint ptr %object to i64
  %base = icmp eq i64 %n, 0
  br i1 %base, label %observe, label %child
child:
  %next = sub i64 %n, 1
  %object.next = inttoptr i64 %next to ptr
  %slot = getelementptr {ptr, ptr}, ptr %task, i32 0, i32 1
  store ptr @start_b, ptr %task, align 8
  store ptr %object.next, ptr %slot, align 8
  %cancel = call i1 (...) @llvm.coro.suspend.retcon.i1(ptr %task)
  br i1 %cancel, label %done, label %observe
observe:
  call void @observe(i64 %n)
  br label %done
done:
  call void @llvm.coro.end(ptr %hdl, i1 false, token none)
  unreachable
}

define {ptr, ptr} @start_b(ptr %buffer, ptr %object) noinline sanitize_address {
  %entry = call ptr @llvm.coro.prepare.retcon(ptr @start_a)
  %result = call {ptr, ptr} %entry(ptr %buffer, ptr %object)
  ret {ptr, ptr} %result
}
