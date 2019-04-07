// 1.2 MHz (default) built in resonator
#define F_CPU 1200000UL
#define boolean int
#define true 1
#define HIGH 1
#define false 0
#define LOW 0

#define btn_1 PB3 // pin2
#define relay_1 PB4 // pin3

#define btn_2 PB2 // pin7
#define relay_2 PB1 //pin6

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

const long interval = 1000;
unsigned long prev_mils = 0;

int btn_1_released = true;
boolean state_1 = LOW;
long duration_1 = 0;
const long period_1 = 4000;
boolean prev_latch_1 = LOW;
boolean latch_1 = LOW;

int btn_2_released = true;
boolean state_2 = LOW;
long duration_2 = 0;
const long period_2 = 4000;
boolean prev_latch_2 = LOW;
boolean latch_2 = LOW;


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

        if(PINB & (1 << btn_1))
        {
            if (btn_1_released) {
                btn_1_released = false;
                delay(100);
                if (HIGH == state_1) {
                    state_1 = LOW;
                    duration_1 = 0;
                } else {
                    state_1 = HIGH;
                    duration_1 = period_1;
                }
            }

        } else {
            if (!btn_1_released) {
                delay(100);
                btn_1_released = true;
            }
            if (state_1 == HIGH) {
                if (cur_mils < prev_mils) {
                    prev_mils = cur_mils;
                } else {
                    unsigned long last_time = cur_mils - prev_mils;
                    if ( last_time >= interval ) {
                        prev_mils = cur_mils;
                        long decremented = duration_1 - interval;
                        if ( decremented < 0 ) {
                            state_1 = LOW;
                        } else {
                            duration_1 = decremented;
                        }
                    }
                }
            }
        }

        latch_1 = state_1;
        if ( latch_1 != prev_latch_1 ) {
            prev_latch_1 = latch_1;
            if (latch_1) {
                PORTB |= (1 << relay_1);    // digitalWrite(relay_1, HIGH);
            } else {
                PORTB &= ~(1 << relay_1);   // digitalWrite(relay_1, LOW);
            }
        }


        if(PINB & (1 << btn_2))
        {
            if (btn_2_released) {
                btn_2_released = false;
                delay(100);
                if (HIGH == state_2) {
                    state_2 = LOW;
                    duration_2 = 0;
                } else {
                    state_2 = HIGH;
                    duration_2 = period_2;
                }
            }

        } else {
            if (!btn_2_released) {
                delay(100);
                btn_2_released = true;
            }
            if (state_2 == HIGH) {
                if (cur_mils < prev_mils) {
                    prev_mils = cur_mils;
                } else {
                    unsigned long last_time = cur_mils - prev_mils;
                    if ( last_time >= interval ) {
                        prev_mils = cur_mils;
                        long decremented = duration_2 - interval;
                        if ( decremented < 0 ) {
                            state_2 = LOW;
                        } else {
                            duration_2 = decremented;
                        }
                    }
                }
            }
        }

        latch_2 = state_2;
        if ( latch_2 != prev_latch_2 ) {
            prev_latch_2 = latch_2;
            if (latch_2) {
                PORTB |= (1 << relay_2);    // digitalWrite(relay_2, HIGH);
            } else {
                PORTB &= ~(1 << relay_2);   // digitalWrite(relay_2, LOW);
            }
        }
    }
}
