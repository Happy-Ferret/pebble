# This is currently broken, as the page tables don't seem to be correct (specificially the identity
# mapping seems to be missing. The reason for this hasn't been found yet. So that needs to be fixed
# and then we should be able to remove the dependency on NASM as far as I can see.
# We should also make all the sections that don't need to be writable just "a" or "ax"

.section .multiboot, "awx"
.intel_syntax noprefix
.code32

# GRUB2 expects tags to be aligned on 8-byte boundaries
# section .multiboot
multiboot_header:
    .long 0xe85250d6                                                           # Multiboot-2 magic
    .long 0                                                                    # Architecture=0 (i386)
    .long multiboot_end - multiboot_header                                     # Header length
    .long 0x100000000-(0xe85250d6 + 0 + (multiboot_end - multiboot_header))    # Checksum

    # Tell GRUB to page-align modules
    .align 8
    .word 6
    .word 0
    .long 8

    # Tell GRUB to switch to 640x480 VGA mode
    .align 8
    .word 5
    .word 0    # Flags
    .long 20   # Size
    .long 640  # Width
    .long 480  # Height
    .long 32   # Depth

    .align 8
    .word 0
    .word 0
    .long 8
multiboot_end:

# The bootstrap is a small physically-mapped bit of code for entering Long Mode and jumping into the
# actual kernel.

# We place the kernel at -2GB because this allows compilers to use R_X86_64_32S relocations to
# address kernel space
.equ KERNEL_VMA, 0xffffffff80000000
.extern _start
.extern _end

# Constants for defining page tables
.equ PAGE_SIZE, 0x1000
.equ PAGE_PRESENT, 0x1
.equ PAGE_WRITABLE, 0x2
.equ PAGE_USER, 0x4
.equ PAGE_HUGE, 0x80
.equ PAGE_NO_EXEC, 0x8000000000000000

.section .bootstrap_data, "awx"
.align 4096
bootstrap_stack_bottom:
    # TODO: create .bootstrap_stack section and reserve bytes
    .rept 4096
        .byte 0
    .endr
bootstrap_stack_top:

# These are the page maps we enter Long Mode with. They have identity mapping set up, along with a
# 1GiB mapping in the higher half starting at 0xffffffff80000000 (P4=511, P3=510, P2=0 [Huge pages]).
boot_pml4:
    .quad (boot_pml3l + PAGE_PRESENT + PAGE_WRITABLE)                  # 0
    .rept (512 - 3)
        .quad 0                                                         # ...
    .endr
    .quad (boot_pml4 + PAGE_PRESENT + PAGE_WRITABLE + PAGE_NO_EXEC)    # 510 - Recursive mapping of the PML4
    .quad (boot_pml3h + PAGE_PRESENT + PAGE_WRITABLE)                  # 511 - Higher-half kernel mapping

boot_pml3l:
    .quad (boot_pml2 + PAGE_PRESENT + PAGE_WRITABLE)                   # 0
    .quad 0                                                            # 1
    .rept (512 - 2)
        .quad 0                                                         # ...
    .endr

boot_pml3h:
    .rept (512 - 2)
        .quad 0                                                         # ...
    .endr
    .quad (boot_pml2 + PAGE_PRESENT + PAGE_WRITABLE)                   # 510
    .quad 0                                                            # 511

boot_pml2:
    .rept 512
        .quad 0
    .endr
    /* .equ PG, 0 */
    /* .rept 512 */
    /*     .quad (PG + PAGE_PRESENT + PAGE_WRITABLE + PAGE_HUGE) */
    /*     .equ PG, (PG+0x200000) */
    /* .endr */
    #%assign pg 0
    #%rep 512
    #    .quad (pg + PAGE_PRESENT + PAGE_WRITABLE + PAGE_HUGE)
    #    %assign pg pg+0x200000
    #%endrep

gdt64:
    .quad 0                                                            # Null selector
    .quad 0x00AF98000000FFFF                                           # CS
    .quad 0x00CF92000000FFFF                                           # DS
gdt64_end:
    .quad 0            # Pad out so .pointer is 16-aligned
gdt64_pointer:
    .word gdt64_end-gdt64-1 # Limit
    .quad gdt64        # Base

.section .bootstrap, "awx"
.code32

# Prints "ERR: " followed by the ASCII character in AL
#     'M' = Incorrect Multiboot magic
#     'C' = CPUID instruction is not supported
#     'L' = Long mode not available
PrintError:
    mov dword ptr [0xb8000], 0x4f524f45
    mov dword ptr [0xb8004], 0x4f3a4f52
    mov dword ptr [0xb8008], 0x4f204f20
    mov byte  ptr [0xb800a], al
    hlt

CheckCpuidSupported:
    pushfd              # Copy EFLAGS into EAX
    pop eax
    mov ecx, eax        # Make a copy in ECX to compare later on
    xor eax, 1<<21      # Flip the ID bit
    push eax            # Copy EAX back into EFLAGS
    popfd
    pushfd              # Read EFLAGS again (with the ID bit flipped or not)
    pop eax
    push ecx            # Restore EFLAGS back to the old version
    popfd

    # Compare the (potentially) flipped version to the first one
    cmp eax, ecx
    je .no_cpuid
    ret
.no_cpuid:
    # TODO: print better error
    mov al, 'C'
    call PrintError

CheckLongModeSupported:
    # Test if we can access the Extended Processor Info
    mov eax, 0x80000000
    cpuid
    cmp eax, 0x80000001
    jb .no_long_mode

    # Check the EPI to see if long mode is available on this CPU
    mov eax, 0x80000001
    cpuid
    test edx, 1<<29
    jz .no_long_mode
    ret
.no_long_mode:
    # TODO: print better error
    mov al, 'L'
    call PrintError

EnablePaging:
    # Populate the level 2 page table
    mov ecx, 0
.populate:
    mov eax, 0x200000
    mul ecx
    or eax, (PAGE_PRESENT + PAGE_WRITABLE + PAGE_HUGE)
    mov [boot_pml2+ecx*8], eax

    inc ecx
    cmp ecx, 512
    jne .populate

    # Load the P4 pointer into CR3
    mov eax, boot_pml4
    mov cr3, eax

    # Enable PAE
    mov eax, cr4
    or eax, 1 << 5
    mov cr4, eax

    # Enable Long-Mode in the EFER MSR
    mov ecx, 0xC0000080
    rdmsr
    or eax, 1 << 8
    wrmsr

    # Enable paging
    mov eax, cr0
    or eax, 1 << 31
    mov cr0, eax

    ret

.global Start
Start:
    mov esp, bootstrap_stack_top
    mov edi, ebx    # Move the pointer to the Multiboot structure into EDI

    # Check that GRUB passed us the correct magic number
    cmp eax, 0x36d76289
    je .multiboot_fine
    # TODO: print better error
    mov al, 'M'
    call PrintError
.multiboot_fine:

    call CheckCpuidSupported
    call CheckLongModeSupported
    call EnablePaging

    # Print 'OK'
    mov dword ptr [0xb8064], 0x2f4b2f4f

    # We're now technically in Long-Mode, but we've been put in 32-bit compatibility submode until we
    # install a valid GDT. We can then far-jump into the new code segment and enter Long Mode for real.
    lgdt [gdt64_pointer]
    #jmp 0x8,Trampoline
    push 0x8
    push Trampoline
    retf

.code64
Trampoline:
    # Reload the GDT pointer with the correct virtual address
    mov rax, [gdt64_pointer + 2]
    mov rbx, KERNEL_VMA
    add rax, rbx
    mov [gdt64_pointer + 2], rax
    mov rax, gdt64_pointer + KERNEL_VMA
    lgdt [rax]

    # Until we install a real GDT, we'll zero all the data and stack selectors. We make sure to leave CS.
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax

    mov rax, qword ptr InHigherHalf
    jmp rax

.section .text, "awx"
.code64
.extern kstart
InHigherHalf:
    # Set up the real stack
    mov rbp, 0          # Terminate stack-traces in the higher-half (makes no sense to go lower)
    mov rsp, _kernel_stack_top

    # Unmap the identity-map and invalidate its TLB entries
    mov qword ptr [boot_pml4], 0x0
    invlpg [0x0]

    # Set the NXE bit in the EFER, to allow use of the No-Execute bit on page table entries
    mov ecx, 0xC0000080
    rdmsr
    or eax, (1<<11)     # Set bit 11 in the lower-part of the EFER
    wrmsr

    # Enable write-protection (bit 16 of CR0)
    mov rax, cr0
    or rax, (1<<16)
    mov cr0, rax

    # Clear RFLAGS
    push 0x0
    popf

    # Call into the kernel
    call kstart

    # If the kernel returns (it shouldn't), put an error value in RAX and loop
    mov rax, 0xDEADBABA
    cli
.loop:
    hlt
    jmp .loop

.section .bss
.align 4096

# We reserve a guard page, which is unmapped when we install the real page tables and will page-fault
# if we overflow the kernel stack
.global _guard_page
.global _kernel_stack_top
_guard_page:
    .lcomm guard_page_bytes, 4096   # Reserve 1 page
stack_bottom:
    .lcomm stack_bytes, 16*4096     # 16 pages = 64KiB
_kernel_stack_top:
