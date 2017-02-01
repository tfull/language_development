bits 64
section .text
global _start

getStringLength:
    mov ecx, 0
gsl_L2:
    mov bl, byte [rax]
    cmp bl, 0
    jz gsl_L1
    inc rax
    inc ecx
    jmp gsl_L2
gsl_L1:
    mov eax, ecx
    ret

getDigits:
    movsx rax, eax
    mov ecx, 0
gd_L1:
    mov rdx, 0
    mov rbx, 10
    idiv rbx
    inc ecx
    cmp eax, 1
    jns gd_L1
    mov eax, ecx
    ret

numericToString:
    mov rsi, rax
    cmp ebx, 0
    jns nts_L1
    neg ebx
    mov dl, '-'
    mov byte [rsi], dl
    inc rsi 
nts_L1:
    push rbx
    push rsi
    mov eax, ebx
    call getDigits
    mov ecx, eax
    pop rsi
    pop rbx
    push rcx
    sub ecx, 1
nts_L2:
    cmp ecx, 0
    js nts_L3
    mov edi, ebx
    mov eax, ebx
    movsx rax, eax
    mov rdx, 0
    mov ebx, 10
    idiv ebx
    push rsi
    movsx rcx, ecx
    add rsi, rcx
    add dl, '0'
    mov byte [rsi], dl
    pop rsi
    mov ebx, eax
    dec ecx
    jmp nts_L2
nts_L3:
    pop rcx
    add rsi, rcx
    mov byte [rsi], 0
    ret

_start:
    mov ebx, 2016
    sub rsp, 12
    mov rax, rsp
    push rax
    call numericToString
    pop rax
    push rax
    call getStringLength
    mov edx, eax
    mov rax, 1
    mov rdi, 1
    pop rsi
    syscall
    mov rax, 1
    mov rdi, 1
    mov rsi, data_newline
    mov rdx, 1
    add rsp, 12
    syscall
    mov rdi, 0
    mov rax, 60
    syscall
    
section .data
    data_newline db 0x0A
