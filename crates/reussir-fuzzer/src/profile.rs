//! Writes the source coverage profile when libFuzzer stops on a crash.
//!
//! The profile runtime writes the `.profraw` file from an exit handler.
//! libFuzzer stops on a crash with `_Exit`, which does not run exit handlers.
//! Thus the harness writes the profile before libFuzzer stops the process.

use std::sync::OnceLock;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::WriteProfileFn;

static WRITE_PROFILE: OnceLock<WriteProfileFn> = OnceLock::new();
static WRITTEN: AtomicBool = AtomicBool::new(false);

/// Writes the profile one time. With `%m` in `LLVM_PROFILE_FILE`, each write
/// adds the counters to the file again.
fn write_once() {
    if let Some(write_profile) = WRITE_PROFILE.get()
        && !WRITTEN.swap(true, Ordering::Relaxed)
    {
        // The runtime prints its own message when the write fails.
        unsafe { write_profile() };
    }
}

/// Keeps `write_profile`, and writes the profile after a panic prints its
/// message. Call this before `libfuzzer_sys::initialize`: its hook calls this
/// hook and then aborts.
pub(crate) fn write_on_panic(write_profile: WriteProfileFn) {
    if WRITE_PROFILE.set(write_profile).is_err() {
        return;
    }
    let print = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        print(info);
        write_once();
    }));
}

/// Writes the profile before the libFuzzer handler of a crash signal runs.
/// libFuzzer sets its handlers after `LLVMFuzzerInitialize`. Thus call this
/// when the first input runs.
#[cfg(unix)]
pub(crate) fn write_on_crash_signal() {
    if WRITE_PROFILE.get().is_some() {
        signals::forward_to_libfuzzer();
    }
}

#[cfg(not(unix))]
pub(crate) fn write_on_crash_signal() {}

#[cfg(unix)]
mod signals {
    use std::ffi::{c_int, c_void};
    use std::sync::atomic::{AtomicUsize, Ordering};

    /// The signals that libFuzzer reports as a crash.
    const SIGNALS: [c_int; 6] = [
        libc::SIGABRT,
        libc::SIGBUS,
        libc::SIGFPE,
        libc::SIGILL,
        libc::SIGSEGV,
        libc::SIGTRAP,
    ];

    static LIBFUZZER_HANDLERS: [AtomicUsize; SIGNALS.len()] =
        [const { AtomicUsize::new(0) }; SIGNALS.len()];

    type Handler = extern "C" fn(c_int, *mut libc::siginfo_t, *mut c_void);

    extern "C" fn write_and_forward(
        signal: c_int,
        info: *mut libc::siginfo_t,
        context: *mut c_void,
    ) {
        super::write_once();
        let Some(index) = SIGNALS.iter().position(|&known| known == signal) else {
            return;
        };
        let handler = LIBFUZZER_HANDLERS[index].load(Ordering::Relaxed);
        // `forward_to_libfuzzer` stores only `SA_SIGINFO` handlers.
        let handler: Handler = unsafe { std::mem::transmute(handler) };
        handler(signal, info, context);
    }

    pub(super) fn forward_to_libfuzzer() {
        for (&signal, slot) in SIGNALS.iter().zip(&LIBFUZZER_HANDLERS) {
            let mut action: libc::sigaction = unsafe { std::mem::zeroed() };
            unsafe { libc::sigaction(signal, std::ptr::null(), &mut action) };
            // Forward only to a handler that uses `SA_SIGINFO`. Keep the other actions.
            if action.sa_flags & libc::SA_SIGINFO == 0 || action.sa_sigaction == 0 {
                continue;
            }
            slot.store(action.sa_sigaction, Ordering::Relaxed);
            action.sa_sigaction = write_and_forward as Handler as libc::sighandler_t;
            unsafe { libc::sigaction(signal, &action, std::ptr::null_mut()) };
        }
    }
}
