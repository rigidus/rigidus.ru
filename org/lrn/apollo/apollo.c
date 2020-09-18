#define F_CPU 1000000UL // 1 MHz

#include <avr/io.h>
#include <util/delay.h>
#include <stdbool.h>
#include <avr/interrupt.h>

#define LOW 0
#define HIGH 1
#define NOT_A_PORT 0
#define PB 1
#define PC 2
#define PD 3

 /*       +-\/-+ */
 /* PC6  1|    |28  PC5 */
 /* PD0  2|    |27  PC4 */
 /* PD1  3|    |26  PC3 */
 /* PD2  4|    |25  PC2 */
 /* PD3  5|    |24  PC1 */
 /* PD4  6|    |23  PC0 */
 /* VCC  7|    |22  GND */
 /* GND  8|    |21  AREF */
 /* PB6  9|    |20  AVCC */
 /* PB7 10|    |19  PB5 */
 /* PD5 11|    |18  PB4 */
 /* PD6 12|    |17  PB3 M */
 /* PD7 13|    |16  PB2 M */
 /* PB0 14|    |15  PB1 */
 /*       +----+ */

const uint8_t digital_pin_to_port[] =
    {
     NOT_A_PORT, /* (0) */
     PC, /* PC6 (1) */
     PD, /* PD0 (2) */
     PD, /* PD1 (3) */
     PD, /* PD2 (4) */
     PD, /* PD3 (5) */
     PD, /* PD4 (6) */
     NOT_A_PORT, /* VCC (7) */
     NOT_A_PORT, /* GND (8) */
     PB, /* PB6 (9)  */
     PB, /* PB7 (10) */
     PD, /* PD5 (11) */
     PD, /* PD6 (12) */
     PD, /* PD7 (13) */
     PB, /* PB0 (14) */
     /* --- */
     PB, /* PB1 (15) */
     PB, /* PB2 (16) */
     PB, /* PB3 (17) */
     PB, /* PB4 (18) */
     PB, /* PB5 (19) */
     NOT_A_PORT, /* AVCC (20) */
     NOT_A_PORT, /* AREF (21) */
     NOT_A_PORT, /* GND (22)  */
     PC, /* PC0 (23) */
     PC, /* PC1 (24) */
     PC, /* PC2 (25) */
     PC, /* PC3 (26) */
     PC, /* PC4 (27) */
     PC, /* PC5 (28) */
    };


const uint8_t digital_pin_to_bit_mask[] =
    {
     0xFF, /* (0) */
     _BV(6), /* (1) */
     _BV(0), /* (2) */
     _BV(1), /* (3) */
     _BV(2), /* (4) */
     _BV(3), /* (5) */
     _BV(4), /* (6) */
     0xFF, /* VCC (7) */
     0xFF, /* GND (8) */
     _BV(6), /* (9)  */
     _BV(7), /* (10) */
     _BV(5), /* (11) */
     _BV(6), /* (12) */
     _BV(7), /* (13) */
     _BV(0), /* (14) */

     _BV(1), /* (15) */
     _BV(2), /* (16) */
     _BV(3), /* (17) */
     _BV(4), /* (18) */
     _BV(5), /* (19) */
     0xFF, /* AVCC (20) */
     0xFF, /* AREF (21) */
     0xFF, /* GND (22)  */
     _BV(0), /* (23) */
     _BV(1), /* (24) */
     _BV(2), /* (25) */
     _BV(3), /* (26) */
     _BV(4), /* (27) */
     _BV(5), /* (28) */
    };



void digitalWrite(uint8_t pin, uint8_t val)
{
    if (pin > 28) return;

    uint8_t port = digital_pin_to_port[pin];

    if (port == NOT_A_PORT) return;

    volatile uint8_t *out;

    switch (port) {
    case PB:
        out = &PORTB;
        break;
    case PC:
        out = &PORTC;
        break;
    case PD:
        out = &PORTD;
        break;
    }

    uint8_t bit = digital_pin_to_bit_mask[pin];

    if (0xFF == bit) return;

    uint8_t oldSREG = SREG;

    cli();

    if (val == LOW) {
        *out &= ~bit;
    } else {
        *out |= bit;
    }

    SREG = oldSREG;
}




typedef unsigned char byte;

/* /\* 7-Segment pin D4 J1 *\/ */
/* #define digit0 14 */
/* /\* 7-Segment pin D3 J4 *\/ */
/* #define digit1 15 */
/* /\* 7-Segment pin D2 J3 *\/ */
/* #define digit2 16 */
/* /\* 7-Segment pin D1 J2 *\/ */
/* #define digit3 17 */


/* /\* 74HC595 pin 10 MR SRCLR J_CLR1 (1) *\/ */
/* #define clockPin = 18; */
/* /\* 74HC595 pin 14 DS J_SER1 (4) *\/ */
/* #define dataPin  = 19; */
/* /\* 74HC595 pin 12 STCP J_CLK1 (2) *\/ */
/* #define latchPin = 13; */

int countdown_base = 90*60+21;
volatile int countdown;
/* byte time_bcd[4]; */
/* unsigned long time = 0; */
bool pulse = 0;
bool mode = 0; // 0 = standart; 1 = edit
int submode = 3;

/* // Hex values reference which LED segments are turned on */
/* // and may vary from circuit to circuit.  Note the mapping above. */
/* byte table[]= { */
/*                0b00111111,  // = 0 */
/*                0b00000110,  // = 1 */
/*                0b01011011,  // = 2 */
/*                0b01001111,  // = 3 */
/*                0b01100110,  // = 4 */
/*                0b01101101,  // = 5 */
/*                0b01111101,  // = 6 */
/*                0b00000111,  // = 7 */
/*                0b01111111,  // = 8 */
/*                0b01101111,  // = 9 */
/*                0b01110111,  // = A */
/*                0b01111100,  // = b */
/*                0b00111001,  // = C */
/*                0b01011110,  // = d */
/*                0b01111001,  // = E */
/*                0b01110001,  // = F */
/*                0b00000000   // blank */
/* }; */

/* #define rows_cnt 4 */
/* #define cols_cnt 6 */

/* /\* входы клавиатуры *\/ */
/* int pinIn  [rows_cnt] = { 5, 4, 3, 2 }; */
/* /\* выходы клавиатуры *\/ */
/* int pinOut [cols_cnt] = { 6, 7, 8, 9, 10, 11 }; */

/* const char value[rows_cnt][cols_cnt] = */
/*     { */
/*      {'7', '8', '9', '/', '%', 'C'}, */
/*      {'4', '5', '6', '*', '^', 'X'}, */
/*      {'1', '2', '3', '-', 'X', '_'}, */
/*      {'X', '0', ',', '+', 'X', '='} */
/*     }; */

/* const int dataPinKeyb  = 12; */
/* const int clockPinKeyb = 10; */
/* const int latchPinKeyb = 11; */


/* 7-Segment pin D4 J1 */
#define display_digit_0 25

/* 7-Segment pin D3 J4 */
#define display_digit_1 24

/* 7-Segment pin D2 J3 */
#define display_digit_2 23

/* 7-Segment pin D1 J2 */
#define display_digit_3 15


const byte control_digit_pins[4] =
    {
     display_digit_3,
     display_digit_2,
     display_digit_1,
     display_digit_0
    };


void display_off () {
    /* turn off all digits */
    for (int x=0; x<4; x++) { // for all four digit
        digitalWrite(control_digit_pins[x], LOW);
    }
}

void display_on () {
    /* turn off all digits */
    for (int x=0; x<4; x++) { // for all four digit
        digitalWrite(control_digit_pins[x], HIGH);
    }
}


/* 74HC595 pin 12 STCP J_CLK1 (2) */
#define display_latch_pin 28

/* 74HC595 pin 10 MR SRCLR J_CLR1 (1) */
#define display_clock_pin 27

/* 74HC595 pin 14 DS J_SER1 (4) */
#define display_data_pin 26


void display_shift_out (uint8_t val)
{
    uint8_t i;

    for (i = 0; i < 8; i++)  {
        if (!!(val & (1 << (7 - i)))) {
            digitalWrite(display_data_pin, HIGH);
        } else {
            digitalWrite(display_data_pin, LOW);
        }
        digitalWrite(display_clock_pin, HIGH);
        digitalWrite(display_clock_pin, LOW);
   }
}



void DisplaySegments(byte displayDigits[4]) {
    for (int x=0; x<4; x++) { // for all four digit
        display_off();
        digitalWrite(display_latch_pin, LOW);  // [=latch down=]
        display_shift_out(displayDigits[x]);
        digitalWrite(display_latch_pin, HIGH); // [=latch up=]
        if (pulse && mode) {
            if (submode == x) {
                digitalWrite(control_digit_pins[x], LOW); // turn off
            } else {
                digitalWrite(control_digit_pins[x], HIGH); // turn on
            }
        } else {
            digitalWrite(control_digit_pins[x], HIGH); // turn on
        }
        _delay_ms(1);
    }
    display_off();
}


void setup() {
    DDRC = 0b111111;

    DDRB = 0b10;
    /* DDRC |= */
    /*     /\* Инициализация пинов семисегментника на PORT_C *\/ */
    /*     (1<<display_clock_pin) && */
    /*     (1<<display_data_pin) && */
    /*     (1<<display_latch_pin) && */
    /*     /\* Инициализация управляющих транзисторных пинов дисплея на PORT_C *\/ */
    /*     (1<<display_digit0) && */
    /*     (1<<display_digit1) && */
    /*     (1<<display_digit2); */
    /* DDRB |= */
    /*     /\* Инициализация последнего транзисторного пинов дисплея на PORT_B *\/ */
    /*     (1<<display_digit3); */

    /* Timer1 установлен на прерывание по переполнению */
    /* Attiny2313 с тактовой частотой 1 МГц. */

    /* Поскольку таймер 16-битный, он может считать до */
    /* максимального значения (2^16 – 1), или 65535. */

    /* При 1 МГц цикл выполняется 1/(1 * 10^6) секунды */

    /* Это означает что 65535 отсчетов произойдут за */
    /* 0.065535 сек. */

    /* Можно использовать делитель, который позволяет  */
    /* поделить тактовый сигнал на различные степени двойки */
    /* и увеличить период таймера. */

    /* В регистре TCCR1B есть три бита CS устанавливающие */
    /* наиболее подходящее разрешение. */
    /* Если установить биты CS10 и CS12 используя: */
    /* TCCR1B |= (1 << CS10); */
    /* TCCR1B |= (1 << CS12); */
    /* то частота тактового источника поделится на 1024. */

    /* Это дает разрешение таймера 1/(1 ∗ 10^6 / 1024) или 0.001024 с. */
    /* Теперь таймер будет переполняться каждые 0.001024*65535 = 67.10784 c */

    /* Но есть и другой режим таймера. */
    /* Он называется сброс таймера по совпадению или CTC. */

    /* В нем таймер сравнивает свой счетчик с переменой сохраненной в регистре. */
    /* Когда счет совпадет с ней, таймер может либо установить флаг, */
    /* либо вызвать прерывание, точно так же как и в случае переполнения. */

    /* Чтобы использовать режим CTC надо понять, сколько циклов нужно, */
    /* чтобы получить интервал в одну секунду. Предположим, что коэффициент */
    /* деления по-прежнему равен 1024 */

    /* Расчет будет следующий: */
    /* (target time) = (timer resolution) * (# timer counts + 1) */
    /* (# timer counts + 1) = (target time) / (timer resolution) */
    /* (# timer counts + 1) = (1 s) / (0.001024 s) */
    /* (# timer counts + 1) = 976.5625 */
    /* (# timer counts) = 976.5625 - 1 = 975.5625 */

    /* Вы должны добавить дополнительную единицу к числу отсчетов */
    /* потому что в CTC режиме при совпадении счетчика с заданным значением */
    /* он сбросит сам себя в ноль. Сброс занимает один тактовый период */
    /*

       (defun resolution (freq div)
         (/ 1
            (/ (* freq (expt 10 6))
               div)))

       (defun cycle-cnt (target-time resolution)
         (- (/ target-time resolution) 1))

        (cycle-cnt 1 (resolution 1 1024))
        => 15609/16 = 975.5625

    */

    /* инициализация Timer1 */
    /* отключить глобальные прерывания */
    cli();
    /* установить регистры в 0 */
    TCCR1A = 0;
    TCCR1B = 0;
    /* установка регистра совпадения */
    OCR1A = 975;
    /* включение в CTC режим */
    TCCR1B |= (1 << WGM12);
    /* Установка битов CS10 и CS12 на коэффициент деления 1024 */
    TCCR1B |= (1 << CS10);
    TCCR1B |= (1 << CS12);
    /* включение прерываний по совпадению */
    TIMSK1 |= (1 << OCIE1A);
    /* включить глобальные прерывания */
    sei();
}

/* bool flag = false; */


int main(void) {
    setup();
    while(1) {
        /* if (flag) { */
        /*     /\* LED on *\/ */
        /*     digitalWrite(15, HIGH); */
        /* } else { */
        /*     /\* LED off *\/ */
        /*     digitalWrite(15, LOW); */
        /* } */

        /* -------------- */
        byte displayDigits[] = { 0b00111111, 0b00111000,
                                 0b01110111, 0b01110110 };

        /* /\* time_bcd is a countdown *\/ */
        /* int_to_time_bcd( countdown, time_bcd ); */
        /* /\* displayDigits is a time_bcd *\/ */
        /* time_bcd_to_time_str( time_bcd, displayDigits ); */

        /* show DisplayDigits */
        DisplaySegments( displayDigits );
    }
    return 0;
}


/* Прерывание по переполнению таймера 1 */
ISR(TIMER1_COMPA_vect)
{
    /* if (flag) { flag = false; } else { flag = true; } */

    if (!mode) {
        if (0 == countdown--) {
            countdown = countdown_base;
        }
    }
}
