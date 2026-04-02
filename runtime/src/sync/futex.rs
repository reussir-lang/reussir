#[cfg(miri)]
mod futex_impl {
    use std::sync::{Mutex};
    use std::ops::Deref;
    use std::sync::atomic::{AtomicU32, Ordering};
    pub struct Futex {
        waiters: Mutex<Vec<std::thread::Thread>>,
        word: AtomicU32
    }

    impl Futex {
        pub fn new(init : u32) -> Self {
            Self {
                waiters: Mutex::new(Vec::new()),
                word: AtomicU32::new(init),
            }
        }
        // can exit spuriously
        pub fn wait(&self, val : u32) {
            if self.word.load(Ordering::Acquire) != val {
                return;
            }
            {
                let mut guard = self.waiters.lock().unwrap();
                if self.word.load(Ordering::Acquire) != val {
                    return;
                }
                guard.push(std::thread::current());
            }
            std::thread::park();
        }

        pub fn notify_one(&self) {
            let mut guard = self.waiters.lock().unwrap();
            if let Some(thread) = guard.pop() {
                thread.unpark();
            }
        }

        pub fn notify_all(&self) {
            let mut guard = self.waiters.lock().unwrap();
            for thread in guard.drain(..) {
                thread.unpark();
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
}

#[cfg(all(not(miri), target_os = "linux"))]
mod futex_impl {
    use std::{ops::Deref, sync::atomic::{AtomicU32, Ordering}};
    use rustix::{io::Errno, thread::futex::Flags};

    #[derive(Default)]
    pub struct Futex(AtomicU32);

    impl Deref for Futex {
        type Target = AtomicU32;
        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl Futex {
        pub fn new(init : u32) -> Self {
            Self(AtomicU32::new(init))
        }

        pub fn wait(&self, val : u32) {
            loop {
                if self.0.load(Ordering::Acquire) != val {
                    return;
                }
                match rustix::thread::futex::wait(&self.0, Flags::PRIVATE, val, None) {
                    Ok(()) => return,
                    Err(Errno::INTR) => continue,
                    Err(_) => return,
                }
            }
        }

        pub fn notify_one(&self) {
            let _ = rustix::thread::futex::wake(&self.0, Flags::PRIVATE, 1);
        }

        pub fn notify_all(&self) {
            let _ = rustix::thread::futex::wake(&self.0, Flags::PRIVATE, i32::MAX as u32);
        }
    }
}

#[cfg(all(not(miri), target_os = "windows"))]
mod futex_impl {
    use std::{ops::Deref, sync::atomic::{AtomicU32, Ordering}};
    use winapi::{
        shared::basetsd::SIZE_T,
        um::{
            synchapi::{WaitOnAddress, WakeByAddressAll, WakeByAddressSingle},
            winbase::INFINITE,
        },
    };

    #[derive(Default)]
    pub struct Futex(AtomicU32);

    impl Deref for Futex {
        type Target = AtomicU32;
        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl Futex {
        pub fn new(init : u32) -> Self {
            Self(AtomicU32::new(init))
        }

        pub fn wait(&self, val : u32) {
            if self.0.load(Ordering::Acquire) != val {
                return;
            }

            let expected = val;
            unsafe {
                let _ = WaitOnAddress(
                    self.0.as_ptr().cast(),
                    std::ptr::from_ref(&expected).cast_mut().cast(),
                    std::mem::size_of::<u32>() as SIZE_T,
                    INFINITE,
                );
            }
        }

        pub fn notify_one(&self) {
            unsafe {
                WakeByAddressSingle(self.0.as_ptr().cast());
            }
        }

        pub fn notify_all(&self) {
            unsafe {
                WakeByAddressAll(self.0.as_ptr().cast());
            }
        }
    }
}

#[cfg(all(not(miri), target_os = "macos"))]
mod futex_impl {
    use std::{
        ffi::{c_int, c_void},
        ops::Deref,
        sync::atomic::{AtomicU32, Ordering},
    };

    const OS_SYNC_WAIT_ON_ADDRESS_NONE: u32 = 0;
    const OS_SYNC_WAKE_BY_ADDRESS_NONE: u32 = 0;
    const EINTR: i32 = 4;
    const EFAULT: i32 = 14;
    const ENOMEM: i32 = 12;

    unsafe extern "C" {
        fn os_sync_wait_on_address(addr: *mut c_void, value: u64, size: usize, flags: u32) -> c_int;
        fn os_sync_wake_by_address_any(addr: *mut c_void, size: usize, flags: u32) -> c_int;
        fn os_sync_wake_by_address_all(addr: *mut c_void, size: usize, flags: u32) -> c_int;
    }

    #[derive(Default)]
    pub struct Futex(AtomicU32);

    impl Deref for Futex {
        type Target = AtomicU32;
        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl Futex {
        pub fn new(init : u32) -> Self {
            Self(AtomicU32::new(init))
        }

        pub fn wait(&self, val : u32) {
            loop {
                if self.0.load(Ordering::Acquire) != val {
                    return;
                }

                let ret = unsafe {
                    os_sync_wait_on_address(
                        self.0.as_ptr().cast(),
                        val as u64,
                        std::mem::size_of::<u32>(),
                        OS_SYNC_WAIT_ON_ADDRESS_NONE,
                    )
                };
                if ret >= 0 {
                    return;
                }

                match std::io::Error::last_os_error().raw_os_error() {
                    Some(EINTR | EFAULT | ENOMEM) => continue,
                    _ => return,
                }
            }
        }

        pub fn notify_one(&self) {
            unsafe {
                let _ = os_sync_wake_by_address_any(
                    self.0.as_ptr().cast(),
                    std::mem::size_of::<u32>(),
                    OS_SYNC_WAKE_BY_ADDRESS_NONE,
                );
            }
        }

        pub fn notify_all(&self) {
            unsafe {
                let _ = os_sync_wake_by_address_all(
                    self.0.as_ptr().cast(),
                    std::mem::size_of::<u32>(),
                    OS_SYNC_WAKE_BY_ADDRESS_NONE,
                );
            }
        }
    }
}

pub use futex_impl::Futex;

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
    fn notify_one_eventually_releases_a_waiter() {
        let futex = Futex::new(0);
        let ready = Barrier::new(2);
        let woke = AtomicBool::new(false);

        thread::scope(|scope| {
            scope.spawn(|| {
                ready.wait();
                futex.wait(0);
                woke.store(true, Ordering::Release);
            });

            ready.wait();
            for _ in 0..1024 {
                if woke.load(Ordering::Acquire) {
                    break;
                }
                futex.notify_one();
                thread::yield_now();
            }

            if !woke.load(Ordering::Acquire) {
                for _ in 0..1024 {
                    futex.notify_all();
                    thread::yield_now();
                    if woke.load(Ordering::Acquire) {
                        break;
                    }
                }
            }
        });

        assert!(woke.load(Ordering::Acquire));
    }

    #[test]
    fn notify_all_eventually_releases_all_waiters() {
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
            for _ in 0..1024 {
                if woke.load(Ordering::Acquire) == waiter_count {
                    break;
                }
                futex.notify_all();
                thread::yield_now();
            }

            if woke.load(Ordering::Acquire) != waiter_count {
                for _ in 0..1024 {
                    futex.notify_all();
                    thread::yield_now();
                    if woke.load(Ordering::Acquire) == waiter_count {
                        break;
                    }
                }
            }
        });

        assert_eq!(woke.load(Ordering::Acquire), waiter_count);
    }
}
