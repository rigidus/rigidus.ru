#define F_CPU 1200000UL

    .section text
    .org 0
    .global init

    rjmp init

init:

main:
    .global main

    rjmp main
