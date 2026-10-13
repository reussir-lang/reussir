//! `rrc`: the Reussir compiler binary — a shim over
//! [`reussir_compiler::driver`], where the whole pipeline lives.

use std::process::ExitCode;

/// Stack of the thread the driver runs on. The parser grows its own stack
/// (`stacker`), but the stages after it walk expression trees recursively
/// too — elaboration, lowering, the syntax tree's drop in `cstree` — at
/// about a kilobyte per nesting level: the process's 8 MiB main thread
/// overflowed on a chain of 8000 `+`. Only touched pages are committed, but
/// the reservation counts against an address-space limit (`ulimit -v`).
const DRIVER_STACK_SIZE: usize = 1024 * 1024 * 1024;

fn main() -> ExitCode {
    // Named `main` so diagnostics (a panic, a stack overflow) read as before.
    let spawned = std::thread::Builder::new()
        .name("main".into())
        .stack_size(DRIVER_STACK_SIZE)
        .spawn(reussir_compiler::driver::main);
    match spawned {
        Ok(driver) => match driver.join() {
            Ok(code) => code,
            // The panic was reported on the driver thread; exit as a
            // panicking main thread would.
            Err(panic) => std::panic::resume_unwind(panic),
        },
        // The stack is reserved address space, so a tight address-space
        // limit (`ulimit -v`) can refuse it: run the driver here, on the
        // main thread's stack, as rrc did before.
        Err(_) => reussir_compiler::driver::main(),
    }
}
