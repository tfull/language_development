bits 64
section .text
global _start

_start:
    mov     rax, 1
    mov     rdi, 1
    mov     rsi, msg
    mov     rdx, len
    syscall
    mov     rdi, 0
    mov     rax, 60
    syscall

section .data
    msg     db      'Hello, world!', 0x0A
    len     equ     $ - msg
