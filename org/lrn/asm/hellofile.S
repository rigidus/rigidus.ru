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
    .ascii "Enter filename: "
    .set buffer_len, . - _buffer
_filename:
    .space 40
    .set filename_len, . - _filename
_real_filename_len:
    .quad 0
_filedescr:
    .quad 0
_filebuffer:
    .space 256
    .set filebuffer_len, . - _filebuffer

    .text

_start:

    # write "hellofile" to stdout
    mov     $sys_write, %rax        # syscall write
    mov     $stdout, %rdi           # stdout
    mov     $_buffer, %rsi          # pointer to buffer
    mov     $buffer_len, %rdx       # lenght of buffer
    syscall

    # read input from the user
    mov     $sys_read, %rax         # syscall read
    mov     $stdin, %rdi            # stdin
    mov     $_filename, %rsi        # pointer to buffer
    mov     $filename_len, %rdx     # lenght of buffer
    syscall
    # Returns the number of bytes that were read.
    # Last symbol is "NewLine". Set it to zero
    dec     %rax
    mov     %rax, _real_filename_len
    add     $_filename, %rax
    xor     %bl, %bl
    mov     %bl, (%rax)

    # open file
    mov     $sys_open, %rax         # syscall open
    mov     $_filename, %rdi        # pointer to buffer
    mov     $0, %rsi                # read and write mode
    mov     $0666, %rdx             # file mode
    syscall

    # check for errors on opening file
    cmp     $0, %rax
    jnl     _save_descr             # ----------------------+
                                    #                       |
_err_open:                          #                       |
    # less than zero, error         #                       |
    # write "open file" to stdout   #                       |
    mov     $sys_write, %rax        # syscall write         |
    mov     $stdout, %rdi           # stdout                |
    mov     $_msg_err_open, %rsi    # pointer to buffer     |
    mov     $msg_err_open_len, %rdx # lenght of buffer      |
    syscall                         #                       |
    # write filename to stdout      #                       |
    mov     $sys_write, %rax        # syscall write         |
    mov     $stdout, %rdi           # stdout                |
    mov     $_filename, %rsi        # pointer to buffer     |
    mov     _real_filename_len, %rdx # lenght of buffer     |
    syscall                         #                       |
    jmp     _err                    # ---> ERR:             |
_msg_err_open:                      #                       |
    .ascii "open file "             #                       |
    .set msg_err_open_len, . - _msg_err_open #              |
                                    #                       |
_save_descr:                        # <---------------------+
    # save filedescr
    mov     %rax, _filedescr        # saving file descriptor into variable
_read:                              # <---------------------------------+
    mov     $sys_read, %rax         # syscall read                      |
    mov     _filedescr, %rdi        # placing file descriptor into      |
    mov     $_filebuffer, %rsi      # moving buffer address to rsi      |
    mov     $256, %rdx              # specity to read 256 bytes         |
    syscall                         #                                   |
                                    #                                   |
    # check for EOF                 #                                   |
    cmp     $0, %rax                #                                   |
    je      _eof                    # ----------------------------------|---+
    jnl     _print                  # ----------------------+           |   |
                                    #                       |           |   |
_err_read:                          #                       |           |   |
    # less than zero, error         #                       |           |   |
    # write "read file" to stdout   #                       |           |   |
    mov     $sys_write, %rax        # syscall write         |           |   |
    mov     $stdout, %rdi           # stdout                |           |   |
    mov     $_msg_err_read, %rsi    # pointer to buffer     |           |   |
    mov     $msg_err_read_len, %rdx # lenght of buffer      |           |   |
    syscall                         #                       |           |   |
    jmp     _err                    # ---> ERR:             |           |   |
_msg_err_read:                      #                       |           |   |
    .ascii "read file "             #                       |           |   |
    .set msg_err_read_len, . - _msg_err_read #              |           |   |
                                    #                       |           |   |
_print:                             # <---------------------+           |   |
    mov     %rax, %rdx              # mov number of bytes read to rdx   |   |
    mov     $sys_write, %rax        # specify print (sys_write) syscall |   |
    mov     $stdout, %rdi           # specify stdout file descriptor    |   |
    mov     $_filebuffer, %rsi      # mov buffer address to rsi         |   |
    syscall                         #                                   |   |
                                    #                                   |   |
    # check for errors              #                                   |   |
    cmp     $0, %rax                #                                   |   |
    jne     _read                   # ----------------------------------+   |
                                    #                                       |
_err_print:                         #                                       |
    # less than zero, error         #                                       |
    # write "print file" to stdout  #                                       |
    mov     $sys_write, %rax        # syscall write                         |
    mov     $stdout, %rdi           # stdout                                |
    mov     $_msg_err_print, %rsi    # pointer to buffer                    |
    mov     $msg_err_print_len, %rdx # lenght of buffer                     |
    syscall                         #                                       |
    jmp     _err                    # ---> ERR:                             |
_msg_err_print:                     #                                       |
    .ascii "print file "            #                                       |
    .set msg_err_print_len, . - _msg_err_print #                            |
                                    #                                       |
_eof:                               # <-------------------------------------+
_close:
    # close and jump to exit
    mov     $sys_close, %rax        # syscall close
    mov     _filedescr, %rdi        # file descriptor
    syscall
    # check for errors
    cmp     $0, %rax
    jnl     _no_error               # ----------------------+
                                    #                       |
_err_close:                         #                       |
    # less than zero, error         #                       |
    # write "close file" to stdout  #                       |
    mov     $sys_write, %rax        # syscall write         |
    mov     $stdout, %rdi           # stdout                |
    mov     $_msg_err_close, %rsi    # pointer to buffer    |
    mov     $msg_err_close_len, %rdx # lenght of buffer     |
    syscall                         #                       |
    jmp     _err                    # ---> ERR:             |
_msg_err_close:                     #                       |
    .ascii "close file "            #                       |
    .set msg_err_close_len, . - _msg_err_close #            |
                                    #                       |
_no_error:                          # <---------------------+
    mov     $0, %rdi

_exit:
    mov     $60, %rax               # system call 60 is exit
    xor     %rdi, %rdi              # we want return code 0
    mov     $0 , %rdi
    syscall

_err:                               # <--- ERR:
    # write "error" to stdout
    mov     $sys_write, %rax        # syscall read
    mov     $stdout, %rdi           # stdout
    mov     $_msg_err, %rsi         # pointer to buffer
    mov     $msg_err_len, %rdx      # lenght of buffer
    syscall
    mov     $-1, %rdi
    jmp     _exit
_msg_err:
    .ascii " error!\n"
    .set msg_err_len, . - _msg_err
