#define F_CPU 1000000UL // 1 MHz

#include <avr/io.h>
#include <util/delay.h>
#include <stdbool.h>

typedef unsigned char byte;

/* 7-Segment pin D4 J1 */
#define digit0 14
/* 7-Segment pin D3 J4 */
#define digit1 15
/* 7-Segment pin D2 J3 */
#define digit2 16
/* 7-Segment pin D1 J2 */
#define digit3 17

byte control_digit_pins[4] = { digit0, digit1, digit2, digit3 };

/* 74HC595 pin 10 MR SRCLR J_CLR1 (1) */
#define clockPin = 18;
/* 74HC595 pin 14 DS J_SER1 (4) */
#define dataPin  = 19;
/* 74HC595 pin 12 STCP J_CLK1 (2) */
#define latchPin = 13;

int countdown_base = 90*60+21;
volatile int countdown;
byte time_bcd[4];
unsigned long time = 0;
bool pulse = 0;
bool mode = 0; // 0 = standart; 1 = edit
int submode = 3;

// Hex values reference which LED segments are turned on
// and may vary from circuit to circuit.  Note the mapping above.
byte table[]= {
               0b00111111,  // = 0
               0b00000110,  // = 1
               0b01011011,  // = 2
               0b01001111,  // = 3
               0b01100110,  // = 4
               0b01101101,  // = 5
               0b01111101,  // = 6
               0b00000111,  // = 7
               0b01111111,  // = 8
               0b01101111,  // = 9
               0b01110111,  // = A
               0b01111100,  // = b
               0b00111001,  // = C
               0b01011110,  // = d
               0b01111001,  // = E
               0b01110001,  // = F
               0b00000000   // blank
};

#define rows_cnt 4
#define cols_cnt 6

/* входы клавиатуры */
int pinIn  [rows_cnt] = { 5, 4, 3, 2 };
/* выходы клавиатуры */
int pinOut [cols_cnt] = { 6, 7, 8, 9, 10, 11 };

const char value[rows_cnt][cols_cnt] =
    {
     {'7', '8', '9', '/', '%', 'C'},
     {'4', '5', '6', '*', '^', 'X'},
     {'1', '2', '3', '-', 'X', '_'},
     {'X', '0', ',', '+', 'X', '='}
    };

const int dataPinKeyb  = 12;
const int clockPinKeyb = 10;
const int latchPinKeyb = 11;


int main(void)
{
    /* LEDs are on portD 0 and 1 */

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
