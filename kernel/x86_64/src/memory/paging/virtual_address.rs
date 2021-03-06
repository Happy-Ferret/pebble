use super::PAGE_SIZE;
use core::cmp::Ordering;
use core::fmt;
use core::ops::{Add, Sub};

#[derive(Clone, Copy)]
pub struct VirtualAddress(pub(super) usize);

impl VirtualAddress {
    pub const fn new(address: usize) -> VirtualAddress {
        VirtualAddress(address)
    }

    pub const fn from_page_table_offsets(
        p4: u16,
        p3: u16,
        p2: u16,
        p1: u16,
        offset: usize,
    ) -> VirtualAddress {
        VirtualAddress::new(
            ((p4 as usize) << 39)
                | ((p3 as usize) << 30)
                | ((p2 as usize) << 21)
                | ((p1 as usize) << 12)
                | (offset << 0),
        ).canonicalise()
    }

    pub const fn ptr<T>(self) -> *const T {
        self.0 as *const T
    }

    pub const fn mut_ptr<T>(self) -> *mut T {
        self.0 as *mut T
    }

    pub const fn offset(&self, offset: isize) -> VirtualAddress {
        VirtualAddress::new(((self.0 as isize) + offset) as usize)
    }

    pub const fn is_page_aligned(&self) -> bool {
        self.0 % PAGE_SIZE == 0
    }

    pub const fn is_aligned_to(&self, alignment: usize) -> bool {
        self.0 % alignment == 0
    }

    pub fn is_in_kernel_space(&self) -> bool {
        self.0 >= ::memory::map::KERNEL_VMA.0 && self.0 <= ::memory::map::KERNEL_SPACE_END.0
    }

    pub const fn offset_into_page(&self) -> usize {
        self.0 % PAGE_SIZE
    }

    /*
     * Addresses are always expected by the CPU to be canonical (bits 48 to 63 are the same as bit
     * 47). If a calculation leaves an address non-canonical, make sure to re-canonicalise it with
     * this function.
     */
    pub const fn canonicalise(self) -> VirtualAddress {
        #[allow(inconsistent_digit_grouping)]
        const SIGN_EXTENSION: usize = 0o177777_000_000_000_000_0000;

        VirtualAddress::new((SIGN_EXTENSION * ((self.0 >> 47) & 0b1)) | (self.0 & ((1 << 48) - 1)))
    }
}

impl fmt::LowerHex for VirtualAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#x}", self.0)
    }
}

impl fmt::UpperHex for VirtualAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#X}", self.0)
    }
}

impl fmt::Debug for VirtualAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#x}", self)
    }
}

impl From<usize> for VirtualAddress {
    fn from(address: usize) -> VirtualAddress {
        VirtualAddress(address)
    }
}

impl From<VirtualAddress> for usize {
    fn from(address: VirtualAddress) -> usize {
        address.0
    }
}

impl<T> From<*const T> for VirtualAddress {
    fn from(ptr: *const T) -> VirtualAddress {
        (ptr as usize).into()
    }
}

impl<T> From<*mut T> for VirtualAddress {
    fn from(ptr: *mut T) -> VirtualAddress {
        (ptr as usize).into()
    }
}

impl Add<VirtualAddress> for VirtualAddress {
    type Output = VirtualAddress;

    fn add(self, rhs: VirtualAddress) -> VirtualAddress {
        (self.0 + rhs.0).into()
    }
}

impl Sub<VirtualAddress> for VirtualAddress {
    type Output = VirtualAddress;

    fn sub(self, rhs: VirtualAddress) -> VirtualAddress {
        (self.0 - rhs.0).into()
    }
}

impl PartialEq<VirtualAddress> for VirtualAddress {
    fn eq(&self, other: &VirtualAddress) -> bool {
        self.0 == other.0
    }
}

impl Eq for VirtualAddress {}

impl PartialOrd<VirtualAddress> for VirtualAddress {
    fn partial_cmp(&self, rhs: &VirtualAddress) -> Option<Ordering> {
        self.0.partial_cmp(&rhs.0)
    }
}

impl Ord for VirtualAddress {
    fn cmp(&self, rhs: &VirtualAddress) -> Ordering {
        self.0.cmp(&rhs.0)
    }
}
