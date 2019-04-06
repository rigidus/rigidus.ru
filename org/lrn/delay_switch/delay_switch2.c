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
#if F_CPU < 150000 && F_CPU > 80000
	x = ovrf * 2;
#elif F_CPU == 600000
	x = ovrf / 2;
#elif F_CPU == 1000000
	x = ovrf / 4;
#elif F_CPU == 1200000
	x = ovrf / 5;
#elif F_CPU == 4000000
	x = ovrf / 16;
#elif F_CPU == 4800000
	x = ovrf / 19;
#elif F_CPU == 8000000
	x = ovrf / 31;
#elif F_CPU == 9600000
	x = ovrf / 37;
#elif F_CPU == 10000000
	x = ovrf / 39;
#elif F_CPU == 12000000
	x = ovrf / 47;
#elif F_CPU == 16000000
	x = ovrf / 63;
#else
#error This CPU frequency is not defined
#endif
	asm("sei");
	return x;
}

void delay(unsigned ms) {
	while(ms--){
		_delay_ms(1);
        //Using the libc routine over and over is non-optimal but it works and is close enough
	} //Note, I may have to reimplement this because the avr-libc delay is too slow *todo*
}


int btn_released = true;
boolean state = LOW;
long duration = 0;
const long period = 4000;
const long interval = 1000;
unsigned long prev_mils = 0;
boolean prev_latch = LOW;
boolean latch = LOW;

int main()
{
	//Setup timer interrupt and PWM pins
	TCCR0B |= _BV(CS00);
	TCCR0A |= _BV(WGM00)|_BV(WGM01);
	TIMSK0 |= 2;
	TCNT0=0;
	sei();
	ADMUX=0;
	//Set up ADC clock depending on F_CPU
#if F_CPU <= 200000
	ADCSRA |= _BV(ADEN);
#elif F_CPU <= 1200000 && F_CPU > 200000
	ADCSRA |= _BV(ADEN) | _BV(ADPS1);
#elif F_CPU > 1200000 && F_CPU < 6400001
	ADCSRA |= _BV(ADEN) | _BV(ADPS2);
#else
	ADCSRA |= _BV(ADEN) | _BV(ADPS1) | _BV(ADPS0) | _BV(ADPS2);
#endif


    DDRB |=  (1 << relay_1);    // pinMode(relay_1, OUTPUT);
    DDRB &= ~(1 << btn_1);      // pinMode(btn_1, INPUT);

    while (1)
    {
        unsigned long cur_mils = millis();

        // Чтение состояния(лог. 1) на порту ввода - вывода (3 вывод порта B):
        if(PINB & (1 << PINB3)) // if (digitalRead(3) == HIGH)
        {
            if (btn_released) {
                btn_released = false;
                delay(100);
                if (HIGH == state) {
                    state = LOW;
                    duration = 0;
                } else {
                    state = HIGH;
                    duration = period;
                }
            }
        } else {
            if (!btn_released) {
                delay(100);
                btn_released = true;
            }
            if (state == HIGH) {
                if (cur_mils < prev_mils) {
                    prev_mils = cur_mils;
                } else {
                    unsigned long last_time = cur_mils - prev_mils;
                    if ( last_time >= interval ) {
                        prev_mils = cur_mils;
                        long decremented = duration - interval;
                        if ( decremented < 0 ) {
                            state = LOW;
                        } else {
                            duration = decremented;
                        }
                    }
                }
            }
        }
        latch = state;
        if ( latch != prev_latch ) {
            prev_latch = latch;
            if (latch) {
                PORTB |= (1 << relay_1);    // digitalWrite(relay_1, HIGH);
            } else {
                PORTB &= ~(1 << relay_1);   // digitalWrite(relay_1, LOW);
            }
        }
    }
}
