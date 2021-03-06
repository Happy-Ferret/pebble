; GRUB2 expects tags to be aligned on 8-byte boundaries
section .multiboot
multiboot_header:
    dd 0xe85250d6                                                           ; Multiboot-2 magic
    dd 0                                                                    ; Architecture=0 (i386)
    dd multiboot_end - multiboot_header                                     ; Header length
    dd 0x100000000-(0xe85250d6 + 0 + (multiboot_end - multiboot_header))    ; Checksum

    ; Tell GRUB to page-align modules
    align 8
    dw 6
    dw 0
    dd 8

    ; Tell GRUB to switch to 640x480 VGA mode
    align 8
    dw 5
    dw 0    ; Flags
    dd 20   ; Size
    dd 640  ; Width
    dd 480  ; Height
    dd 32   ; Depth

    align 8
    dw 0
    dw 0
    dd 8
multiboot_end:

; The bootstrap is a small physically-mapped bit of code for entering Long Mode and jumping into the
; actual kernel.

; We place the kernel at -2GB because this allows compilers to use R_X86_64_32S relocations to
; address kernel space
KERNEL_VMA equ 0xFFFFFFFF80000000
extern _start
extern _end

; Constants for defining page tables
PAGE_SIZE       equ 0x1000
PAGE_PRESENT    equ 0x1
PAGE_WRITABLE   equ 0x2
PAGE_USER       equ 0x4
PAGE_HUGE       equ 0x80
PAGE_NO_EXEC    equ 0x8000000000000000

section .bootstrap_data
align 4096
bootstrap_stack_bottom:
    times 4096 db 0     ; This should really be `resb`d in a BSS section, but that was effort
bootstrap_stack_top:

; These are the page maps we enter Long Mode with. They have identity mapping set up, along with a
; 1GiB mapping in the higher half starting at 0xffffffff80000000 (P4=511, P3=510, P2=0 [Huge pages]).
boot_pml4:
    dq (boot_pml3l + PAGE_PRESENT + PAGE_WRITABLE)                  ; 0
    times (512 - 3) dq 0                                            ; ...
    dq (boot_pml4 + PAGE_PRESENT + PAGE_WRITABLE + PAGE_NO_EXEC)    ; 510 - Recursive mapping of the PML4
    dq (boot_pml3h + PAGE_PRESENT + PAGE_WRITABLE)                  ; 511 - Higher-half kernel mapping

boot_pml3l:
    dq (boot_pml2 + PAGE_PRESENT + PAGE_WRITABLE)                   ; 0
    dq 0                                                            ; 1
    times (512 - 2) dq 0                                            ; ...

boot_pml3h:
    times (512 - 2) dq 0                                            ; ...
    dq (boot_pml2 + PAGE_PRESENT + PAGE_WRITABLE)                   ; 510
    dq 0                                                            ; 511

boot_pml2:
    %assign pg 0
    %rep 512
        dq (pg + PAGE_PRESENT + PAGE_WRITABLE + PAGE_HUGE)
        %assign pg pg+0x200000
    %endrep

gdt64:
    dq 0                                                            ; Null selector
    dq 0x00AF98000000FFFF                                           ; CS
    dq 0x00CF92000000FFFF                                           ; DS
.end:
    dq 0            ; Pad out so .pointer is 16-aligned
.pointer:
    dw .end-gdt64-1 ; Limit
    dq gdt64        ; Base

section .bootstrap
bits 32

; Prints "ERR: " followed by the ASCII character in AL
;     'M' = Incorrect Multiboot magic
;     'C' = CPUID instruction is not supported
;     'L' = Long mode not available
PrintError:
    mov dword [0xb8000], 0x4f524f45
    mov dword [0xb8004], 0x4f3a4f52
    mov dword [0xb8008], 0x4f204f20
    mov byte  [0xb800a], al
    hlt

CheckCpuidSupported:
    pushfd              ; Copy EFLAGS into EAX
    pop eax
    mov ecx, eax        ; Make a copy in ECX to compare later on
    xor eax, 1<<21      ; Flip the ID bit
    push eax            ; Copy EAX back into EFLAGS
    popfd
    pushfd              ; Read EFLAGS again (with the ID bit flipped or not)
    pop eax
    push ecx            ; Restore EFLAGS back to the old version
    popfd

    ; Compare the (potentially) flipped version to the first one
    cmp eax, ecx
    je .no_cpuid
    ret
.no_cpuid:
    ; TODO: print better error
    mov al, 'C'
    call PrintError

CheckLongModeSupported:
    ; Test if we can access the Extended Processor Info
    mov eax, 0x80000000
    cpuid
    cmp eax, 0x80000001
    jb .no_long_mode

    ; Check the EPI to see if long mode is available on this CPU
    mov eax, 0x80000001
    cpuid
    test edx, 1<<29
    jz .no_long_mode
    ret
.no_long_mode:
    ; TODO: print better error
    mov al, 'L'
    call PrintError

EnablePaging:
    ; Load the P4 pointer into CR3
    mov eax, boot_pml4
    mov cr3, eax

    ; Enable PAE
    mov eax, cr4
    or eax, 1 << 5
    mov cr4, eax

    ; Enable Long-Mode in the EFER MSR
    mov ecx, 0xC0000080
    rdmsr
    or eax, 1 << 8
    wrmsr

    ; Enable paging
    mov eax, cr0
    or eax, 1 << 31
    mov cr0, eax

    ret

global Start
Start:
    mov esp, bootstrap_stack_top
    mov edi, ebx    ; Move the pointer to the Multiboot structure into EDI

    ; Check that GRUB passed us the correct magic number
    cmp eax, 0x36d76289
    je .multiboot_fine
    ; TODO: print better error
    mov al, 'M'
    call PrintError
.multiboot_fine:

    call CheckCpuidSupported
    call CheckLongModeSupported
    call EnablePaging

    ; Print 'OK' for early debugging perposes
    mov dword [0xb8064], 0x2f4b2f4f

    ; We're now technically in Long-Mode, but we've been put in 32-bit compatibility submode until we
    ; install a valid GDT. We can then far-jump into the new code segment and enter Long Mode for real.
    lgdt [gdt64.pointer]
    jmp 0x8:Trampoline

bits 64
Trampoline:
    ; Reload the GDT pointer with the correct virtual address
    mov rax, [gdt64.pointer + 2]
    mov rbx, KERNEL_VMA
    add rax, rbx
    mov [gdt64.pointer + 2], rax
    mov rax, gdt64.pointer + KERNEL_VMA
    lgdt [rax]

    ; Until we install a real GDT, we'll zero all the data and stack selectors. We make sure to leave CS.
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax

    mov rax, qword InHigherHalf
    jmp rax

section .text
bits 64
extern kstart
InHigherHalf:
    ; Set up the real stack
    mov rbp, 0          ; Terminate stack-traces in the higher-half (makes no sense to go lower)
    mov rsp, _kernel_stack_top

    ; Unmap the identity-map and invalidate its TLB entries
    mov qword [boot_pml4], 0x0
    invlpg [0x0]

    ; Set the NXE bit in the EFER, to allow use of the No-Execute bit on page table entries
    mov ecx, 0xC0000080
    rdmsr
    or eax, (1<<11)     ; Set bit 11 in the lower-part of the EFER
    wrmsr

    ; Enable write-protection (bit 16 of CR0)
    mov rax, cr0
    or rax, (1<<16)
    mov cr0, rax

    ; Clear RFLAGS
    push 0x0
    popf

    ; Call into the kernel
    call kstart

    ; If the kernel returns (it shouldn't), put an error value in RAX and loop
    mov rax, 0xDEADBABA
    cli
.loop:
    hlt
    jmp .loop

section .bss
align 4096

; We reserve a guard page, which is unmapped when we install the real page tables and will page-fault
; if we overflow the kernel stack
global _guard_page
global _kernel_stack_top
_guard_page:
    resb 4096       ; 1 page
stack_bottom:
    resb 16*4096    ; 16 pages = 64KiB
_kernel_stack_top:
