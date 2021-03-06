use core::marker::PhantomData;
use core::ops::{Index, IndexMut};
use memory::map::P4_TABLE_ADDRESS;
use memory::paging::entry::{Entry, EntryFlags};
use memory::paging::{VirtualAddress, ENTRY_COUNT};
use memory::FrameAllocator;

pub enum Level4 {}
pub enum Level3 {}
pub enum Level2 {}
pub enum Level1 {}

pub trait TableLevel {}

impl TableLevel for Level4 {}
impl TableLevel for Level3 {}
impl TableLevel for Level2 {}
impl TableLevel for Level1 {}

/*
 * Tables of levels implementing HierarchicalLevel are page tables whose children are other tables
 * (as opposed to actual frames (like in P1 tables)). This allows us to restrict certain operations
 * (such as getting the next level of page tables) to tables with child tables.
 */
pub trait HierarchicalLevel: TableLevel {
    type NextLevel: TableLevel;
}

impl HierarchicalLevel for Level4 {
    type NextLevel = Level3;
}
impl HierarchicalLevel for Level3 {
    type NextLevel = Level2;
}
impl HierarchicalLevel for Level2 {
    type NextLevel = Level1;
}

pub struct Table<L: TableLevel> {
    entries: [Entry; ENTRY_COUNT],
    level: PhantomData<L>,
}

/// This is the **currently installed** P4.
pub const P4: *mut Table<Level4> = P4_TABLE_ADDRESS.mut_ptr();

impl<L> Table<L>
where
    L: TableLevel,
{
    pub fn zero(&mut self) {
        for entry in self.entries.iter_mut() {
            entry.set_unused();
        }
    }
}

impl<L> Table<L>
where
    L: HierarchicalLevel,
{
    fn next_table_address(&self, index: u16) -> Option<VirtualAddress> {
        let entry_flags = self[index].flags();

        if entry_flags.contains(EntryFlags::PRESENT) && !entry_flags.contains(EntryFlags::HUGE_PAGE)
        {
            /*
             * We can calculate the next table's address by going through one more layer of the
             * recursive mapping.
             *
             * XXX: This can make the address non-canonical, as we shift the old table address up
             *      into the old sign-extension, so we make sure to canonicalise it again.
             */
            let table_address = (self as *const _) as usize;
            Some(
                VirtualAddress::new((table_address << 9) | (usize::from(index) << 12))
                    .canonicalise(),
            )
        } else {
            None
        }
    }

    pub fn next_table(&self, index: u16) -> Option<&Table<L::NextLevel>> {
        self.next_table_address(index)
            .map(|address| unsafe { &*address.ptr() })
    }

    pub fn next_table_mut(&mut self, index: u16) -> Option<&mut Table<L::NextLevel>> {
        self.next_table_address(index)
            .map(|address| unsafe { &mut *address.mut_ptr() })
    }

    pub fn next_table_create(
        &mut self,
        index: u16,
        user_accessible: bool,
        allocator: &mut FrameAllocator,
    ) -> &mut Table<L::NextLevel> {
        if self.next_table(index).is_none() {
            assert!(
                !self.entries[index as usize]
                    .flags()
                    .contains(EntryFlags::HUGE_PAGE),
                "mapping code does not support huge pages"
            );
            let frame = allocator.allocate_frame().expect("no frames available");

            self.entries[index as usize].set(
                frame,
                EntryFlags::default() | EntryFlags::WRITABLE | if user_accessible {
                    EntryFlags::USER_ACCESSIBLE
                } else {
                    EntryFlags::empty()
                },
            );

            self.next_table_mut(index).unwrap().zero();
        }

        self.next_table_mut(index).unwrap()
    }
}

impl<L> Index<u16> for Table<L>
where
    L: TableLevel,
{
    type Output = Entry;

    fn index(&self, index: u16) -> &Entry {
        &self.entries[index as usize]
    }
}

impl<L> IndexMut<u16> for Table<L>
where
    L: TableLevel,
{
    fn index_mut(&mut self, index: u16) -> &mut Entry {
        &mut self.entries[index as usize]
    }
}
