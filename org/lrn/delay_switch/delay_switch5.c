// 1.2 MHz (default) built in resonator
#define F_CPU 1200000UL

#include <avr/io.h>
#include <util/delay.h>
#include <avr/interrupt.h>


#define LED_PIN PB5 | PB0

ISR(TIM0_OVF_vect)
{

    PORTB ^= _BV(LED_PIN); // toggle LED pin
}

int
main(void)
{

    /* setup */
    DDRB = 0b00111111; // set LED pin as OUTPUT
    PORTB = 0b00111111; // set all pins to LOW
    TCCR0B |= _BV(CS02)|_BV(CS00); // set prescaler to 1024 (CLK=1200000Hz/1024/256=4Hz, 0.25s)
    TIMSK0 |= _BV(TOIE0); // enable Timer Overflow interrupt
    sei(); // enable global interrupts

    /* loop */
    while (1);
}
