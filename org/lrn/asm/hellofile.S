# ----------------------------------------------------------------------------------------
#     gcc -c hellofile.S && ld hellofile.o && ./a.out
# or
#     gcc -nostdlib hellofile.S && ./a.out
# ----------------------------------------------------------------------------------------
.global _start

    #define sys_read 0
    #define sys_write 1
    #define sys_open 2
    #define sys_close 3

    #define stdin 0
    #define stdout 1

    .data

_buffer:
    .ascii "Hellofile! Enter filename: "
    .set buffer_len, . - _buffer
_msg_err_open:
    .ascii "open file "
    .set msg_err_open_len, . - _msg_err_open
_msg_err_read:
    .ascii "read file "
    .set msg_err_read_len, . - _msg_err_read
_msg_err_print:
    .ascii "print file "
    .set msg_err_print_len, . - _msg_err_print
_msg_err_close:
    .ascii "close file "
    .set msg_err_close_len, . - _msg_err_close
_msg_err:
    .ascii " error!\n"
    .set msg_err_len, . - _msg_err
_filename_len:
.quad 0
_filedescr:
    .quad 0
_filebuffer:
    .space 256
    .set filebuffer_len, . - _filebuffer

    .text

_start:

    # write "hellofile" to stdout
    mov     $sys_write, %rax        # syscall read
    mov     $stdout, %rdi           # stdout
    mov     $_buffer, %rsi          # pointer to buffer
    mov     $buffer_len, %rdx       # lenght of buffer
    syscall

    # read input from the user
    mov     $sys_read, %rax         # syscall read
    mov     $stdin, %rdi            # stdin
    mov     $_buffer, %rsi          # pointer to buffer
    mov     $buffer_len, %rdx       # lenght of buffer
    syscall
    # Returns the number of bytes that were read.
    # Last symbol is "NewLine". Set it to zero
    dec     %rax
    mov     %rax, _filename_len
    add     $_buffer, %rax
    xor     %bl, %bl
    mov     %bl, (%rax)

    # open file
    mov     $sys_open, %rax         # syscall open
    mov     $_buffer, %rdi          # pointer to buffer
    mov     $0, %rsi                # read and write mode
    mov     $0666, %rdx             # file mode
    syscall

    # check for errors on opening file
    cmp     $0, %rax
    jl      _err_open               # if less than zero, error
    mov     %rax, _filedescr        # saving file descriptor into variable

_read:
    mov     $sys_read, %rax         # syscall read
    mov     _filedescr, %rdi        # placing file descriptor into
    mov     $_filebuffer, %rsi      # moving buffer address to rsi
    mov     $256, %rdx              # specity to read 256 bytes
    syscall

    # check for EOF
    cmp     $0, %rax
    jl      _err_read               # if less than zero, error
    jne     _print                  # if not EOF, jump over to print
_close:
    # close and jump to exit
    mov     $sys_close, %rax        # syscall close
    mov     _filedescr, %rdi        # file descriptor
    syscall
    # check for errors
    cmp     $0, %rax
    jl      _err_close              # if less than zero, error
    jmp     _exit

_print:
    mov     %rax, %rdx              # mov number of bytes read to rdx
    mov     $sys_write, %rax        # specify print (sys_write) syscall
    mov     $stdout, %rdi           # specify stdout file descriptor
    mov     $_filebuffer, %rsi      # mov buffer address to rsi to prepare for printing syscall
    syscall

    # check for errors
    cmp     $0, %rax
    jl      _err_print              # if less than zero, error
    jmp     _read                   # jump back to read for another loop

_err_open:
    # write "open file" to stdout
    mov     $sys_write, %rax        # syscall write
    mov     $stdout, %rdi           # stdout
    mov     $_msg_err_open, %rsi    # pointer to buffer
    mov     $msg_err_open_len, %rdx # lenght of buffer
    syscall
    # write filename to stdout
    mov     $sys_write, %rax        # syscall write
    mov     $stdout, %rdi           # stdout
    mov     $_buffer, %rsi          # pointer to buffer
    mov     _filename_len, %rdx     # lenght of buffer
    syscall
    jmp     _err

_err_read:
    # write "read file" to stdout
    mov     $sys_write, %rax        # syscall write
    mov     $stdout, %rdi           # stdout
    mov     $_msg_err_read, %rsi    # pointer to buffer
    mov     $msg_err_read_len, %rdx # lenght of buffer
    syscall
    jmp     _err

_err_print:
    # write "print file" to stdout
    mov     $sys_write, %rax        # syscall write
    mov     $stdout, %rdi           # stdout
    mov     $_msg_err_print, %rsi    # pointer to buffer
    mov     $msg_err_print_len, %rdx # lenght of buffer
    syscall
    jmp     _err

_err_close:
    # write "close file" to stdout
    mov     $sys_write, %rax        # syscall write
    mov     $stdout, %rdi           # stdout
    mov     $_msg_err_close, %rsi    # pointer to buffer
    mov     $msg_err_close_len, %rdx # lenght of buffer
    syscall
    jmp     _err

_err:
    # write "error" to stdout
    mov     $sys_write, %rax        # syscall read
    mov     $stdout, %rdi           # stdout
    mov     $_msg_err, %rsi         # pointer to buffer
    mov     $msg_err_len, %rdx      # lenght of buffer
    syscall
    jmp     _exit

_no_error:
    mov     $0, %rdi

_exit:
    mov     $60, %rax               # system call 60 is exit
    xor     %rdi, %rdi              # we want return code 0
    mov     $0 , %rdi
    syscall
