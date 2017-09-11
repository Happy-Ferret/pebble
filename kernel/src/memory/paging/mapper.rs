/*
 * Copyright (C) 2017, Isaac Woods.
 * See LICENCE.md
 */

use super::{VirtualAddress,PhysicalAddress,Page,PAGE_SIZE,ENTRY_COUNT};
use super::entry::*;
use super::table::{self,Table,Level4};
use memory::{Frame,FrameAllocator};
use core::ptr::Unique;

pub struct Mapper
{
    p4 : Unique<Table<Level4>>,
}

impl Mapper
{
    pub unsafe fn new() -> Mapper
    {
        Mapper { p4 : Unique::new_unchecked(table::P4) }
    }

    pub fn p4(&self) -> &Table<Level4>
    {
        unsafe { self.p4.as_ref() }
    }

    pub fn p4_mut(&mut self) -> &mut Table<Level4>
    {
        unsafe { self.p4.as_mut() }
    }

    pub fn translate(&self, virtual_address : VirtualAddress) -> Option<PhysicalAddress>
    {
        let offset = virtual_address % PAGE_SIZE;
        self.translate_page(Page::get_containing_page(virtual_address)).map(|frame| frame.number * PAGE_SIZE + offset)
    }

    pub fn translate_page(&self, page : Page) -> Option<Frame>
    {
        let p3 = self.p4().next_table(page.p4_index());

        let huge_page =
            || {
                p3.and_then(
                    |p3| {
                        let p3_entry = &p3[page.p3_index()];
                        // 1GiB page?
                        if let Some(start_frame) = p3_entry.get_pointed_frame()
                        {
                            if p3_entry.flags().contains(HUGE_PAGE)
                            {
                                assert!(start_frame.number % (ENTRY_COUNT * ENTRY_COUNT) == 0);
                                return Some(Frame
                                            {
                                                number : start_frame.number + page.p2_index() * ENTRY_COUNT + page.p1_index()
                                            });
                            }
                        }

                        if let Some(p2) = p3.next_table(page.p3_index())
                        {
                            let p2_entry = &p2[page.p2_index()];
                            // 2MiB page?
                            if let Some(start_frame) = p2_entry.get_pointed_frame()
                            {
                                if p2_entry.flags().contains(HUGE_PAGE)
                                {
                                    // address must be 2MiB aligned
                                    assert!(start_frame.number % ENTRY_COUNT == 0);
                                    return Some(Frame { number : start_frame.number + page.p1_index() });
                                }
                            }
                        }
                        None
                    })
            };
    
        p3.and_then(|p3| p3.next_table(page.p3_index()))
          .and_then(|p2| p2.next_table(page.p2_index()))
          .and_then(|p1| p1[page.p1_index()].get_pointed_frame())
          .or_else(huge_page)
    }

    pub fn map<A>(&mut self, page : Page, flags : EntryFlags, allocator : &mut A) where A : FrameAllocator
    {
        let frame = allocator.allocate_frame().expect("out of memory");
        self.map_to(page, frame, flags, allocator)
    }

    pub fn identity_map<A>(&mut self, frame : Frame, flags : EntryFlags, allocator : &mut A) where A : FrameAllocator
    {
        let page = Page::get_containing_page(frame.get_start_address());
        self.map_to(page, frame, flags, allocator);
    }

    pub fn unmap<A>(&mut self, page : Page, allocator : &mut A) where A : FrameAllocator
    {
        assert!(self.translate(page.get_start_address()).is_some());

        let p1 = self.p4_mut()
                     .next_table_mut(page.p4_index())
                     .and_then(|p3| p3.next_table_mut(page.p3_index()))
                     .and_then(|p2| p2.next_table_mut(page.p2_index()))
                     .expect("we don't support huge pages");
        let frame = p1[page.p1_index()].get_pointed_frame().unwrap();
        p1[page.p1_index()].set_unused();
    
        // Clear the TLB entry for this page
        use x86_64::instructions::tlb;
        use x86_64::VirtualAddress;
        tlb::flush(VirtualAddress(page.get_start_address()));

        // TODO free p(1,2,3) table if it has become empty
//        allocator.deallocate_frame(frame); TODO
    }

    pub fn map_to<A>(&mut self, page : Page, frame : Frame, flags : EntryFlags, allocator : &mut A) where A : FrameAllocator
    {
        let p4 = self.p4_mut();
        let p3 = p4.next_table_create(page.p4_index(), allocator);
        let p2 = p3.next_table_create(page.p3_index(), allocator);
        let p1 = p2.next_table_create(page.p2_index(), allocator);
    
//        println!("Eat a dick: Frame : {:#x} Page: {:#x}", page.get_start_address(), page.get_start_address());
//        println!("P1 index: {:#x}", page.p1_index());
        assert!(p1[page.p1_index()].is_unused());
        p1[page.p1_index()].set(frame, flags | PRESENT);
    }
}