#define F_CPU 1000000UL // 1 MHz

#include <avr/io.h>
#include <util/delay.h>

int main(void)
{
    // LEDs are on portD 0 and 1
    DDRB = 0x17;

    while(1)
        {
            PORTB = 0x01;
            _delay_ms(500);
            PORTB = 0x02;
            _delay_ms(500);
            PORTB = 0x03;
            _delay_ms(500);
            PORTB = 0x00;
            _delay_ms(500);
        }
}
