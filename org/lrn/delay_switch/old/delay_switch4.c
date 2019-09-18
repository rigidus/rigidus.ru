// 1.2 MHz (default) built in resonator
#define F_CPU 1200000UL
#define boolean int
#define true 1
#define HIGH 1
#define false 0
#define LOW 0

#define btn_1 PB3
#define relay_1 PB4

#include <avr/io.h>
#include <util/delay.h>
#include <avr/interrupt.h>

volatile unsigned long ovrf=0;
ISR(TIM0_OVF_vect){
    ovrf++; //Increment counter every 256 clock cycles
}

unsigned long millis(){
    unsigned long x;
    asm("cli");
    /*Scale number of timer overflows to milliseconds*/
    x = ovrf / 5;
    asm("sei");
    return x;
}

void delay(unsigned ms) {
    while(ms--){
        _delay_ms(1);
    }
}

int main()
{
    //Setup timer interrupt and PWM pins
    TCCR0B |= _BV(CS00);
    TCCR0A |= _BV(WGM00)|_BV(WGM01);
    TIMSK0 |= 2;
    TCNT0=0;
    sei();

    DDRB |=  (1 << relay_1);    // pinMode(relay_1, OUTPUT);
    DDRB &= ~(1 << btn_1);      // pinMode(btn_1, INPUT);

    while (1)
    {
        unsigned long cur_mils = millis();
    }
}
