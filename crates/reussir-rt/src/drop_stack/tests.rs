use super::*;

struct Chain {
    remaining: usize,
}
unsafe extern "C" fn chain(object: *mut u8) {
    let chain = unsafe { &mut *object.cast::<Chain>() };
    if chain.remaining != 0 {
        chain.remaining -= 1;
        unsafe { defer(object, self::chain) };
    }
}

#[test]
fn deep_work_uses_a_bounded_native_stack() {
    std::thread::Builder::new()
        .stack_size(64 * 1024)
        .spawn(|| {
            let mut object = Chain {
                remaining: 1_000_000,
            };
            unsafe { run((&mut object as *mut Chain).cast(), chain) };
            assert_eq!(object.remaining, 0);
        })
        .unwrap()
        .join()
        .unwrap();
}

struct Ordered {
    log: Vec<usize>,
}
unsafe extern "C" fn first(object: *mut u8) {
    unsafe { (*object.cast::<Ordered>()).log.push(1) };
}
unsafe extern "C" fn defer_first(object: *mut u8) {
    unsafe { defer(object, first) };
}
unsafe extern "C" fn nested(object: *mut u8) {
    unsafe { defer(object, first) };
    unsafe { run(object, defer_first) };
    unsafe { (*object.cast::<Ordered>()).log.push(2) };
}

#[test]
fn public_nested_calls_complete_only_their_suffix() {
    let mut object = Ordered { log: vec![] };
    unsafe { run((&mut object as *mut Ordered).cast(), nested) };
    assert_eq!(object.log, [1, 2, 1]);
}
