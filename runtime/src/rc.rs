use std::{
    cell::{Cell, UnsafeCell},
    marker::PhantomData,
    mem::ManuallyDrop,
    ptr::NonNull,
};

#[repr(C)]
struct RcBox<T> {
    count: Cell<usize>,
    data: UnsafeCell<T>,
}

// we cannot use NonNull here as want to keep the layout of the box the same as raw pointer
#[repr(transparent)]
pub struct Rc<T>(*mut RcBox<T>);

// Rc without ownership
#[repr(transparent)]
pub struct RcRef<'a, T>(ManuallyDrop<Rc<T>>, PhantomData<&'a T>);

impl<T> Rc<T> {
    pub fn new(data: T) -> Self {
        let boxed = Box::new(RcBox {
            count: Cell::new(1),
            data: UnsafeCell::new(data),
        });
        let ptr = Box::leak(boxed);
        Self(ptr)
    }
    // effectively performs check in debug mode and annotates nonnull in release mode
    unsafe fn get_box(&self) -> NonNull<RcBox<T>> {
        unsafe { NonNull::new_unchecked(self.0 as *mut RcBox<T>) }
    }
    pub fn data_ref(&self) -> &T {
        unsafe { &*self.get_box().as_ref().data.get() }
    }
    pub fn data_ptr(&self) -> NonNull<T> {
        NonNull::from(self.data_ref())
    }
    pub fn count_ref(&self) -> &Cell<usize> {
        unsafe { &self.get_box().as_ref().count }
    }
    pub fn is_unique(&self) -> bool {
        self.count_ref().get() == 1
    }
    pub fn as_ref<'a>(&'a self) -> RcRef<'a, T> {
        RcRef(ManuallyDrop::new(Self(self.0)), PhantomData)
    }
}

impl<T> Clone for Rc<T> {
    fn clone(&self) -> Self {
        let count = self.count_ref().get();
        self.count_ref().set(count + 1);
        Self(self.0)
    }
}

impl<T> Drop for Rc<T> {
    fn drop(&mut self) {
        let count = self.count_ref().get();
        self.count_ref().set(count - 1);
        if count == 0 {
            _ = unsafe { Box::from_raw(self.0) };
        }
    }
}
impl<T: Clone> Rc<T> {
    pub fn make_mut(&mut self) -> &mut T {
        if self.is_unique() {
            unsafe { self.data_ptr().as_mut() }
        } else {
            let data = self.data_ref().clone();
            *self = Self::new(data);
            unsafe { self.data_ptr().as_mut() }
        }
    }
}

impl<T> std::ops::Deref for Rc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.data_ref()
    }
}

impl<T> std::ops::Deref for RcRef<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0.data_ref()
    }
}
