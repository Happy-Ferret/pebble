use bit_field::BitField;
use cpu;
use interrupts::InterruptStackFrame;
use local_apic::LocalApic;
use port::Port;

pub static mut PIT: Pit = Pit::new();

pub struct Pit {
    sleeping: bool,
    command_port: Port<u8>,
    channel_0: Port<u8>,
}

/// This is the frequency the PIT oscillator runs at in Hz.
const PIT_FREQUENCY: usize = 1_193_180;

impl Pit {
    const fn new() -> Pit {
        Pit {
            sleeping: false,
            command_port: unsafe { Port::new(0x43) },
            channel_0: unsafe { Port::new(0x40) },
        }
    }

    /// Initialise the PIC to run at `frequency` Hz. Note that the hardware may only support
    /// certain frequencies.
    pub fn init(&mut self, frequency: usize) {
        let divisor = PIT_FREQUENCY / frequency;

        unsafe {
            /*
             * Tell the PIC that we're gonna specify both bytes of a Mode 3 divisor (to generate a
             * square wave) on Channel 0.
             */
            self.command_port.write(0b0011_0110);

            self.channel_0.write(divisor.get_bits(0..8) as u8); // Write low byte
            self.channel_0.write(divisor.get_bits(8..16) as u8); // Write high byte
        }
    }

    /// Sleep for `duration` milliseconds.
    /// This puts the PIC in Mode 0. Afterwards, it will not send interrupts.
    /// Re-call `init()` to put it back in Mode 3.
    pub fn do_sleep(&mut self, duration: usize) {
        const TICKS_IN_ONE_MS: usize = PIT_FREQUENCY / 1000;
        let counter_value = TICKS_IN_ONE_MS * duration;

        unsafe {
            /*
             * Tell the PIC we're gonna specify both bytes of a Mode 0 counter value on Channel 0.
             */
            self.command_port.write(0b0011_0000);

            /*
             * Specify the counter value to count down from. This also starts the countdown.
             */
            self.sleeping = false;
            self.channel_0.write(counter_value.get_bits(0..8) as u8); // Write low byte
            self.channel_0.write(counter_value.get_bits(8..16) as u8); // Write high byte

            /*
             * When the count down reaches zero, an interrupt will be sent, since we're on Channel
             * 0, so we spinlock until we get the interrupt, then return.
             */
            #[allow(while_immutable_condition)]
            while self.sleeping {
                cpu::wait_for_interrupt();
            }
        }
    }
}

/// Handler for interrupts from the Programmable Interrupt Controller.
/// **Should not be called manually!**
pub extern "C" fn pit_handler(_: &InterruptStackFrame) {
    unsafe {
        if PIT.sleeping {
            PIT.sleeping = false;
        }

        LocalApic::send_eoi();
    }
}
