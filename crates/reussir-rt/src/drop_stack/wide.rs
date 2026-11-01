use super::{Object, Release};
use core::{num::NonZeroU8, ptr};
use smallvec::SmallVec;

#[derive(Clone, Copy)]
pub(super) struct ReleaseIndex(NonZeroU8);

#[derive(Clone, Copy)]
struct CachedRelease {
    release: Release,
    index: ReleaseIndex,
}

#[derive(Default)]
pub(super) struct ReleaseTable {
    releases: SmallVec<[Release; 8]>,
    cache: [Option<CachedRelease>; 16],
}

impl ReleaseTable {
    pub fn index(&mut self, release: Release) -> Option<ReleaseIndex> {
        let slot = (release as usize >> 4) % self.cache.len();
        if let Some(cached) = self.cache[slot]
            && ptr::fn_addr_eq(cached.release, release)
        {
            return Some(cached.index);
        }
        let position = self
            .releases
            .iter()
            .position(|known| ptr::fn_addr_eq(*known, release));
        let position = match position {
            Some(position) => position,
            None if self.releases.len() < u8::MAX as usize => {
                let position = self.releases.len();
                self.releases.push(release);
                position
            }
            None => return None,
        };
        let index = ReleaseIndex(NonZeroU8::new((position + 1) as u8).unwrap());
        self.cache[slot] = Some(CachedRelease { release, index });
        Some(index)
    }

    pub fn get(&self, index: ReleaseIndex) -> Release {
        self.releases[index.0.get() as usize - 1]
    }

    pub fn clear(&mut self) {
        // Every cached index names an entry in releases.
        if self.releases.is_empty() {
            return;
        }
        self.releases.clear();
        self.cache.fill(None);
        if self.releases.capacity() > 64 {
            self.releases.shrink_to_fit();
        }
    }
}

pub(super) struct Link {
    pub previous: Object,
    pub release: ReleaseIndex,
}

/// An exclusively owned header: u32 count followed by padding or a u16 tag.
pub(super) struct WideHeader(Object);

impl WideHeader {
    pub unsafe fn new(object: Object) -> Self {
        Self(object)
    }

    fn upper(&self) -> *mut u16 {
        self.0
            .as_ptr()
            .wrapping_add(if cfg!(target_endian = "little") { 6 } else { 4 })
            .cast()
    }

    pub unsafe fn mark_cell(&self) {
        unsafe { self.upper().write(0) };
    }

    pub unsafe fn link(&self, previous: Object, release: ReleaseIndex) -> bool {
        let address = self.0.0.as_ptr().addr();
        let distance = previous.0.as_ptr().addr().wrapping_sub(address);
        let words = (distance as isize >> 3) as i64;
        if address as u64 >> 56 != 0 || distance & 7 != 0 || words << 24 >> 24 != words {
            return false;
        }
        let upper = ((words >> 32) as u16 & 0xff) | ((release.0.get() as u16) << 8);
        unsafe {
            self.0.as_ptr().cast::<u32>().write(words as u32);
            self.upper().write(upper);
        }
        previous.0.as_ptr().expose_provenance();
        true
    }

    pub unsafe fn take_link(&self) -> Option<Link> {
        let upper = unsafe { self.upper().read() };
        let release = ReleaseIndex(NonZeroU8::new((upper >> 8) as u8)?);
        let lower = unsafe { self.0.as_ptr().cast::<u32>().read() };
        unsafe {
            self.0.as_ptr().cast::<u32>().write(1);
            self.upper().write(0);
        }
        let words = (((upper as u64 & 0xff) << 32) | lower as u64) as i64;
        let words = words << 24 >> 24;
        let address = self
            .0
            .0
            .as_ptr()
            .addr()
            .wrapping_add((words as usize).wrapping_mul(8));
        let previous = unsafe { Object::new(ptr::with_exposed_provenance_mut(address)) };
        Some(Link { previous, release })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    unsafe extern "C" fn first(_: *mut u8) {}
    unsafe extern "C" fn second(_: *mut u8) {
        std::hint::black_box(());
    }

    #[test]
    fn a_full_function_table_uses_existing_entries_only() {
        let mut table = ReleaseTable::default();
        let known: Release = first;
        table.releases.resize(u8::MAX as usize, known);
        assert!(table.index(known).is_some());
        assert!(table.index(second).is_none());
    }

    #[test]
    fn cleared_tables_can_reassign_cached_indices() {
        let mut table = ReleaseTable::default();
        let first: Release = first;
        let second: Release = second;
        let index = table.index(first).unwrap();
        assert!(ptr::fn_addr_eq(table.get(index), first));
        table.clear();
        table.clear();
        let index = table.index(second).unwrap();
        assert!(ptr::fn_addr_eq(table.get(index), second));
        let index = table.index(first).unwrap();
        assert!(ptr::fn_addr_eq(table.get(index), first));
    }

    #[test]
    fn links_require_an_aligned_representable_offset() {
        let mut words = [1_u32, 0, 1, 0];
        let object = unsafe { Object::new(words.as_mut_ptr().cast()) };
        let unaligned = unsafe { Object::new(words.as_mut_ptr().add(1).cast()) };
        let header = unsafe { WideHeader::new(object) };
        let mut table = ReleaseTable::default();
        let index = table.index(first).unwrap();
        assert!(!unsafe { header.link(unaligned, index) });
        assert_eq!(words, [1, 0, 1, 0]);
    }
}
