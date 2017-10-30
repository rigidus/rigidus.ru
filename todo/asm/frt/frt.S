/* http://demin.ws/blog/russian/2012/09/27/fcode/ */

.macro next
    jmp     NEXT$
.endm


GetNext$:
    cld
    mov      IP$, si
    lodsw
    mov      si, IP$
    retn

CALLR$:
    add      2, RP$
    mov      RP$, bp
    mov      IP$, ax
    mov      ax, (bp)
    popw     IP$
    next

RETR$:
    mov      RP$, bp
    mov      (bp), ax
    mov      ax, IP$
    sub      2, RP$
    next

NEXT$:
    call    GetNext$
    jmp     ax

osPush$:
    call    GetNext$
    push    ax
    next



    ;;  Adc  ( a b -> c isCarry )
    ;;  if a+b>FFFF isCarry = FFFF else isCarry=0

osAdc$:
    pop     ax  dx ; -> a b
    add     ax, dx
    sbb     dx, dx
    push    ax  dx ; c isCarry ->
    NEXT

    ;;  osSwap ( a b -> b a )

osSwap$:
    pop      ax bx
    push     ax bx
    NEXT

    ;;  osRot ( a b c -> b c a )

osRot$:
    pop      ax bx cx
    push     bx ax cx
    NEXT

osPut$:
    add     RP$, 2
    mov     bp, RP$
    pop     word ptr [bp]
    NEXT

osGet$:
    mov     bp, RP$
    push    word ptr [bp]
    sub     RP$, 2
    NEXT

osDrop$:
    add     sp, 2
    NEXT

    ;;  osNor ( a b -> a NOR b )

osNor$:
    pop     ax bx
    or      ax, bx
    not     ax
    push    ax
    NEXT

osTrap$:
    int     3
    NEXT

    ;;  osPeek ( addr -> value )

osPeek$:
    pop     bx
    push    word ptr [bx]
    NEXT

    ;;  osPoke ( Value Addr -> )

osPoke$:
    pop     bx  ; -> Value Addr
    pop     word ptr [bx] ; ->
                    NEXT
