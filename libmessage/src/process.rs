/// Every process has a Send Buffer and a Receive Buffer. To send a message, the process pushes it
/// into the Send Buffer. The kernel then empties this buffer, parses each message and routes it to
/// the right place.
///
/// To send this process a message, the kernel places it in the Receive Buffer. The process should
/// empty this queue periodically and handle each message within it.
///
/// Each buffer starts with a `u16` offset into the buffer that marks the buffer's tail. This
/// allows us to both easily add new messages at the end of the buffer, and know when to stop
/// when we're processing messages from the buffer. These offsets must be operated on atomically,
/// and only when the new message has been fully encoded into the buffer (in the case of adding a
/// message) or (in the case of removing a message from the buffer) when we no longer intend to
/// access the message's memory.

/// Every process must be able to respond to these messages.
pub enum ProcessMessage {}

/*
 * TODO
 * We still need to decide how to manage these buffers. It'd be nice to not have to use locks
 * between the kernel and process, but also be able to push into it from multiple process threads.
 * It'd probably just be better to build each message then copy it into the buffer when we know
 * it's length for now.
 */
use core::{mem, ptr};
use serde::{Deserialize, Serialize};
use serializer::MessageSerializer;
use {Error, MessageHeader, MessageWriter, NodeId, Result};

pub const MESSAGE_BUFFER_SIZE: usize = 4096; // in bytes
                                             // TODO: decide on real buffer addresses (maybe lay out entire userspace address space)
pub const SEND_BUFFER_ADDRESS: usize = 0xff_0000_0000;

#[repr(C, packed)]
pub struct SendBuffer {
    /// This is the current tail of the buffer.
    // TODO: not convinced this is that elegant. We need to find a way to mark a message complete
    // WARNING CRAZY IDEA THAT'S PROBABLY ENORMOURSLY UNSAFE: Because we fill in the header last,
    // we can just say -> if you can't find the next header, there's not a message (even if we're
    // actually done serializing it into the buffer). We do either need to atomically copy the
    // header in, or lock the buffer during that.
    // XXX: We could actually just have a byte at the front of this that must to atomically written
    // to, that marks if we're in the middle of writing into the buffer. The kernel could just not
    // read the buffers that pre-emption if the "busy" byte is set.
    pub buffer_tail: u16,
}

impl SendBuffer {
    /// Get a reference to the Send Buffer. Unsafe because this allows us to create multiple
    /// mutable references, which we musn't do.
    pub unsafe fn new() -> &'static mut SendBuffer {
        &mut *(SEND_BUFFER_ADDRESS as *mut SendBuffer)
    }

    /// Send a message of type `M`. `M` must be both `Serialize`, but also `Deserialize`, because
    /// the kernel must be able to decode it.
    pub fn send<M>(&mut self, destination: NodeId, message: &M) -> Result<()>
    where
        M: Serialize + for<'de> Deserialize<'de>,
    {
        // Reserve space for the message header
        let header_offset = self.buffer_tail;
        self.buffer_tail += mem::size_of::<MessageHeader>() as u16;

        // Serialize the message in
        {
            let mut serializer = MessageSerializer::new(self);
            message.serialize(&mut serializer)?;
        }

        // Now we know how much space we used, we can fill in the header
        let header = MessageHeader {
            destination,
            payload_length: self.buffer_tail
                - header_offset
                - mem::size_of::<MessageHeader>() as u16,
        };

        unsafe {
            ptr::copy(
                &header as *const MessageHeader as *const u8,
                (SEND_BUFFER_ADDRESS + mem::size_of::<SendBuffer>() + header_offset as usize)
                    as *mut u8,
                mem::size_of::<MessageHeader>(),
            );
        }

        Ok(())
    }
}

/*
 * We write into the Send Buffer from the userspace process.
 */
impl MessageWriter for SendBuffer {
    fn write_u8(&mut self, value: u8) -> Result<()> {
        if usize::from(self.buffer_tail) + 1 > MESSAGE_BUFFER_SIZE {
            return Err(Error::SendBufferFull);
        }

        unsafe {
            let buffer_ptr =
                (self as *mut SendBuffer as *mut u8).offset(mem::size_of::<SendBuffer>() as isize);
            ptr::write_volatile(buffer_ptr.offset(self.buffer_tail as isize), value);
            self.buffer_tail += 1;
        }

        Ok(())
    }
}

/*
 * We read from the Send Buffer in the kernel.
 */
// pub trait MessageReader {
//     fn read_u8(&self) -> Result<u8> {

//     }
// }
