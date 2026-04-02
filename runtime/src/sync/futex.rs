use std::ops::Deref;
use std::sync::atomic::{AtomicU32, Ordering};

#[cfg(all(not(miri), target_os = "linux"))]
use rustix::{io::Errno, thread::futex::Flags};
#[cfg(all(not(miri), target_os = "macos"))]
use std::ffi::{c_int, c_void};
#[cfg(all(not(miri), target_os = "windows"))]
use winapi::{
    shared::basetsd::SIZE_T,
    um::{
        synchapi::{WaitOnAddress, WakeByAddressAll},
        winbase::INFINITE,
    },
};

#[cfg(all(not(miri), target_os = "macos"))]
const OS_SYNC_WAIT_ON_ADDRESS_NONE: u32 = 0;
#[cfg(all(not(miri), target_os = "macos"))]
const OS_SYNC_WAKE_BY_ADDRESS_NONE: u32 = 0;
#[cfg(all(not(miri), target_os = "macos"))]
const EINTR: i32 = 4;
#[cfg(all(not(miri), target_os = "macos"))]
const EFAULT: i32 = 14;
#[cfg(all(not(miri), target_os = "macos"))]
const ENOMEM: i32 = 12;

#[cfg(all(not(miri), target_os = "macos"))]
unsafe extern "C" {
    fn os_sync_wait_on_address(addr: *mut c_void, value: u64, size: usize, flags: u32) -> c_int;
    fn os_sync_wake_by_address_all(addr: *mut c_void, size: usize, flags: u32) -> c_int;
}

pub struct Futex {
    word: AtomicU32,
}

impl Futex {
    pub fn new(init: u32) -> Self {
        Self {
            word: AtomicU32::new(init),
        }
    }

    pub fn wait(&self, expected: u32) {
        #[cfg(any(
            miri,
            not(any(target_os = "linux", target_os = "windows", target_os = "macos"))
        ))]
        {
            while self.word.load(Ordering::Acquire) == expected {
                std::hint::spin_loop();
            }
        }

        #[cfg(all(not(miri), target_os = "linux"))]
        loop {
            if self.word.load(Ordering::Acquire) != expected {
                return;
            }
            match rustix::thread::futex::wait(&self.word, Flags::PRIVATE, expected, None) {
                Ok(()) | Err(Errno::INTR) | Err(Errno::AGAIN) => continue,
                Err(_) => return,
            }
        }

        #[cfg(all(not(miri), target_os = "windows"))]
        loop {
            if self.word.load(Ordering::Acquire) != expected {
                return;
            }

            let wait_succeeded = unsafe {
                WaitOnAddress(
                    self.word.as_ptr().cast(),
                    std::ptr::from_ref(&expected).cast_mut().cast(),
                    std::mem::size_of::<u32>() as SIZE_T,
                    INFINITE,
                )
            } != 0;

            if !wait_succeeded {
                return;
            }
        }

        #[cfg(all(not(miri), target_os = "macos"))]
        loop {
            if self.word.load(Ordering::Acquire) != expected {
                return;
            }

            let ret = unsafe {
                os_sync_wait_on_address(
                    self.word.as_ptr().cast(),
                    expected as u64,
                    std::mem::size_of::<u32>(),
                    OS_SYNC_WAIT_ON_ADDRESS_NONE,
                )
            };
            if ret >= 0 {
                continue;
            }

            match std::io::Error::last_os_error().raw_os_error() {
                Some(EINTR | EFAULT | ENOMEM) => continue,
                _ => return,
            }
        }
    }

    pub fn wake_with(&self, message: u32) {
        #[cfg(any(
            miri,
            not(any(target_os = "linux", target_os = "windows", target_os = "macos"))
        ))]
        {
            self.word.store(message, Ordering::Release);
        }

        #[cfg(all(not(miri), target_os = "linux"))]
        {
            self.word.store(message, Ordering::Release);
            let _ = rustix::thread::futex::wake(&self.word, Flags::PRIVATE, i32::MAX as u32);
        }

        #[cfg(all(not(miri), target_os = "windows"))]
        {
            self.word.store(message, Ordering::Release);
            unsafe {
                WakeByAddressAll(self.word.as_ptr().cast());
            }
        }

        #[cfg(all(not(miri), target_os = "macos"))]
        {
            self.word.store(message, Ordering::Release);
            unsafe {
                let _ = os_sync_wake_by_address_all(
                    self.word.as_ptr().cast(),
                    std::mem::size_of::<u32>(),
                    OS_SYNC_WAKE_BY_ADDRESS_NONE,
                );
            }
        }
    }
}

impl Deref for Futex {
    type Target = AtomicU32;

    fn deref(&self) -> &Self::Target {
        &self.word
    }
}

impl Default for Futex {
    fn default() -> Self {
        Self::new(0)
    }
}

#[cfg(test)]
mod tests {
    use super::Futex;
    use std::sync::Barrier;
    use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
    use std::thread;

    #[test]
    fn default_initializes_to_zero() {
        let futex = Futex::default();
        assert_eq!(futex.load(Ordering::Acquire), 0);
    }

    #[test]
    fn new_initializes_to_value() {
        let futex = Futex::new(7);
        assert_eq!(futex.load(Ordering::Acquire), 7);
    }

    #[test]
    fn wait_returns_immediately_for_mismatched_value() {
        let futex = Futex::new(1);
        let returned = AtomicBool::new(false);

        thread::scope(|scope| {
            scope.spawn(|| {
                futex.wait(0);
                returned.store(true, Ordering::Release);
            });
        });

        assert!(returned.load(Ordering::Acquire));
    }

    #[test]
    fn wake_with_same_value_does_not_release_a_waiter() {
        let futex = Futex::new(0);
        let ready = Barrier::new(2);
        let woke = AtomicBool::new(false);
        let mut woke_early = false;

        thread::scope(|scope| {
            scope.spawn(|| {
                ready.wait();
                futex.wait(0);
                woke.store(true, Ordering::Release);
            });

            ready.wait();
            for _ in 0..1024 {
                futex.wake_with(0);
                thread::yield_now();
            }

            woke_early = woke.load(Ordering::Acquire);
            futex.wake_with(1);
        });

        assert!(!woke_early);
        assert!(woke.load(Ordering::Acquire));
        assert_eq!(futex.load(Ordering::Acquire), 1);
    }

    #[test]
    fn wake_with_releases_all_waiters() {
        let futex = Futex::new(0);
        let waiter_count = 3;
        let ready = Barrier::new(waiter_count + 1);
        let woke = AtomicUsize::new(0);

        thread::scope(|scope| {
            for _ in 0..waiter_count {
                scope.spawn(|| {
                    ready.wait();
                    futex.wait(0);
                    woke.fetch_add(1, Ordering::AcqRel);
                });
            }

            ready.wait();
            futex.wake_with(1);
        });

        assert_eq!(woke.load(Ordering::Acquire), waiter_count);
        assert_eq!(futex.load(Ordering::Acquire), 1);
    }
}
