use super::{Object, RETAINED_BUFFER_CAPACITY, Release};
use core::{
    hash::{Hash, Hasher},
    num::NonZeroU8,
    ptr,
};
use indexmap::IndexSet;
use rustc_hash::FxBuildHasher;

/// Compare and hash callbacks by address while retaining the callable pointer.
#[derive(Clone, Copy)]
struct ReleaseKey(Release);

impl PartialEq for ReleaseKey {
    fn eq(&self, other: &Self) -> bool {
        ptr::fn_addr_eq(self.0, other.0)
    }
}

impl Eq for ReleaseKey {}

impl Hash for ReleaseKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.0 as usize).hash(state);
    }
}

/// One-based callback index; zero is reserved for a pointer-cell link.
#[derive(Clone, Copy)]
pub(super) struct ReleaseIndex(NonZeroU8);

#[derive(Clone, Copy)]
struct CachedRelease {
    release: Release,
    index: ReleaseIndex,
}

/// Callback indices shared by linked runs until the outermost drain finishes.
/// The small direct-mapped cache avoids repeated searches for recent callbacks.
/// Entries remain in insertion order until clear so header indices stay valid.
#[derive(Default)]
pub(super) struct ReleaseTable {
    releases: IndexSet<ReleaseKey, FxBuildHasher>,
    cache: [Option<CachedRelease>; 16],
}

impl ReleaseTable {
    /// Reuse or assign a byte-sized index. A full table still resolves known
    /// callbacks, but returns None for a new one so the caller can start a run.
    pub fn index(&mut self, release: Release) -> Option<ReleaseIndex> {
        let slot = (release as usize >> 4) % self.cache.len();
        if let Some(cached) = self.cache[slot]
            && ptr::fn_addr_eq(cached.release, release)
        {
            return Some(cached.index);
        }
        let key = ReleaseKey(release);
        let position = if self.releases.len() < u8::MAX as usize {
            self.releases.insert_full(key).0
        } else {
            self.releases.get_index_of(&key)?
        };
        let index = ReleaseIndex(NonZeroU8::new((position + 1) as u8).unwrap());
        self.cache[slot] = Some(CachedRelease { release, index });
        Some(index)
    }

    /// Resolve an index issued by this table since its last clear.
    pub fn get(&self, index: ReleaseIndex) -> Release {
        self.releases[index.0.get() as usize - 1].0
    }

    /// Discard indices after all linked runs finish, retaining bounded capacity.
    pub fn clear(&mut self) {
        // Every cached index names an entry in releases.
        if self.releases.is_empty() {
            return;
        }
        self.releases.clear();
        self.cache.fill(None);
        if self.releases.capacity() > RETAINED_BUFFER_CAPACITY {
            self.releases.shrink_to(RETAINED_BUFFER_CAPACITY);
        }
    }
}

/// The previous pending object and the callback needed to release it.
pub(super) struct Link {
    pub previous: Object,
    pub release: ReleaseIndex,
}

/// An exclusively owned header: u32 count followed by padding or a u16 tag.
/// A link uses the count and the next word's upper 16 bits for a signed 40-bit
/// offset in eight-byte units plus an 8-bit callback index. The tag is preserved.
pub(super) struct WideHeader(Object);

impl WideHeader {
    /// View a newly dead box or an already encoded pending link. The caller must
    /// guarantee exclusive access and the header layout described above.
    pub unsafe fn new(object: Object) -> Self {
        Self(object)
    }

    /// Locate the spare half of the second word without touching its low tag bits.
    fn upper(&self) -> *mut u16 {
        self.0
            .as_ptr()
            .wrapping_add(if cfg!(target_endian = "little") { 6 } else { 4 })
            .cast()
    }

    /// Mark that the previous object is in Pending::cells and shares this callback.
    pub unsafe fn mark_cell(&self) {
        unsafe { self.upper().write(0) };
    }

    /// Encode the previous object and its callback, exposing its pointer provenance
    /// for reconstruction. Leave the header unchanged if the link cannot fit.
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

    /// Decode a link and restore count=1 and clear spare bits before release.
    /// A zero callback index leaves the header untouched and selects Pending::cells.
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
        unsafe extern "C" fn callback<const N: usize>(_: *mut u8) {
            std::hint::black_box(N);
        }
        macro_rules! callbacks {
            ($($n:expr),*) => { [$(callback::<$n> as Release),*] };
        }
        let callbacks = callbacks!(
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
            24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
            46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67,
            68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89,
            90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108,
            109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125,
            126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142,
            143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
            160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176,
            177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193,
            194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210,
            211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227,
            228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244,
            245, 246, 247, 248, 249, 250, 251, 252, 253, 254
        );
        let mut table = ReleaseTable::default();
        for (position, callback) in callbacks.into_iter().enumerate() {
            assert_eq!(
                table.index(callback).unwrap().0.get() as usize,
                position + 1
            );
        }
        assert!(table.index(second).is_none());
        for (position, callback) in callbacks.into_iter().enumerate().rev() {
            let index = table.index(callback).unwrap();
            assert_eq!(index.0.get() as usize, position + 1);
            assert!(ptr::fn_addr_eq(table.get(index), callback));
        }
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
