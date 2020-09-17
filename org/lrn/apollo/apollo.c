#define F_CPU 1000000UL // 1 MHz

#include <avr/io.h>
#include <util/delay.h>
#include <stdbool.h>
#include <avr/interrupt.h>

/* typedef unsigned char byte; */

/* /\* 7-Segment pin D4 J1 *\/ */
/* #define digit0 14 */
/* /\* 7-Segment pin D3 J4 *\/ */
/* #define digit1 15 */
/* /\* 7-Segment pin D2 J3 *\/ */
/* #define digit2 16 */
/* /\* 7-Segment pin D1 J2 *\/ */
/* #define digit3 17 */

/* byte control_digit_pins[4] = { digit0, digit1, digit2, digit3 }; */

/* /\* 74HC595 pin 10 MR SRCLR J_CLR1 (1) *\/ */
/* #define clockPin = 18; */
/* /\* 74HC595 pin 14 DS J_SER1 (4) *\/ */
/* #define dataPin  = 19; */
/* /\* 74HC595 pin 12 STCP J_CLK1 (2) *\/ */
/* #define latchPin = 13; */

/* int countdown_base = 90*60+21; */
/* volatile int countdown; */
/* byte time_bcd[4]; */
/* unsigned long time = 0; */
/* bool pulse = 0; */
/* bool mode = 0; // 0 = standart; 1 = edit */
/* int submode = 3; */

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


void setup() {
    /* set PB0 to output */
    DDRB |= 1<<PB0;

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

bool flag = false;


int main(void) {
    setup();
    while(1) {
        if (flag) {
            /* LED on */
            PORTB &= ~(1<<PB0);
        } else {
            /* LED off */
            PORTB |= 1<<PB0;
        }
    }
    return 0;
}


/* Прерывание по переполнению таймера 1 */
ISR(TIMER1_COMPA_vect)
{
    if (flag) { flag = false; } else { flag = true; }

    /* if (!mode) { */
    /*     if (0 == countdown--) { */
    /*         countdown = countdown_base; */
    /*     } */
    /* } */
}
