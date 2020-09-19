#define F_CPU 1000000UL // 1 MHz

#include <avr/io.h>
#include <util/delay.h>
#include <stdbool.h>
#include <avr/interrupt.h>

#define LOW 0
#define HIGH 1
#define NOT_A_PORT 0xF
#define NOT_A_MASK 0xF
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

const uint8_t pin_to_port_and_mask[] =
    {
     ((NOT_A_PORT << 4) | NOT_A_MASK), /* (0) */
     ((PC << 4) | PC6 ), /* (1) */
     ((PD << 4) | PD0 ), /* (2) */
     ((PD << 4) | PD1 ), /* (3) */
     ((PD << 4) | PD2 ), /* (4) */
     ((PD << 4) | PD3 ), /* (5) */
     ((PD << 4) | PD4 ), /* (6) */
     ((NOT_A_PORT << 4) | NOT_A_MASK), /* VCC (7) */
     ((NOT_A_PORT << 4) | NOT_A_MASK), /* GND (8) */
     ((PB << 4) | PB6 ), /* (9) */
     ((PB << 4) | PB7 ), /* (10) */
     ((PD << 4) | PD5 ), /* (11) */
     ((PD << 4) | PD6 ), /* (12) */
     ((PD << 4) | PD7 ), /* (13) */
     ((PB << 4) | PB0 ), /* (14) */
     /* --- */
     ((PB << 4) | PB1 ), /* (15) */
     ((PB << 4) | PB2 ), /* (16) */
     ((PB << 4) | PB3 ), /* (17) */
     ((PB << 4) | PB4 ), /* (18) */
     ((PB << 4) | PB5 ), /* (19) */
     ((NOT_A_PORT << 4) | NOT_A_MASK), /* AVCC (20) */
     ((NOT_A_PORT << 4) | NOT_A_MASK), /* AREF (21) */
     ((NOT_A_PORT << 4) | NOT_A_MASK), /* GND (22) */
     ((PC << 4) | PC0 ), /* (23) */
     ((PC << 4) | PC1 ), /* (24) */
     ((PC << 4) | PC2 ), /* (25) */
     ((PC << 4) | PC3 ), /* (26) */
     ((PC << 4) | PC4 ), /* (27) */
     ((PC << 4) | PC5 ), /* (28) */
    };


int8_t pin_write ( uint8_t pin, uint8_t val ) {
    if (pin > sizeof( pin_to_port_and_mask )) return -1;

    uint8_t port = ( pin_to_port_and_mask[pin] >> 4) ;

    volatile uint8_t *port_pnt;

    switch (port) {
    case PB:
        port_pnt = &PORTB;
        break;
    case PC:
        port_pnt = &PORTC;
        break;
    case PD:
        port_pnt = &PORTD;
        break;
    default:
        return -1;
    }

    uint8_t mask = ( pin_to_port_and_mask[pin] & 0x0F );
    if (NOT_A_MASK == mask) return -1;

    uint8_t bit  = (1 << mask);

    uint8_t oldSREG = SREG;

    cli();

    if (val == LOW) {
        *port_pnt &= ~bit;
    } else {
        *port_pnt |= bit;
    }

    SREG = oldSREG;
}


int8_t pin_read ( uint8_t pin ) {
    if (pin > sizeof( pin_to_port_and_mask )) return -1;

    uint8_t port = ( pin_to_port_and_mask[pin] >> 4) ;
    uint8_t *port_pnt;

    switch (port) {
    case PB:
        port_pnt = &PINB;
        break;
    case PC:
        port_pnt = &PINC;
        break;
    case PD:
        port_pnt = &PIND;
        break;
    default:
        return -1;
    }

    uint8_t mask = ( pin_to_port_and_mask[pin] & 0x0F );
    if (NOT_A_MASK == mask) return -1;

    if ( 0 == ( (*port_pnt) & ( 1 << mask ) ) ) {
        return LOW;
    }
    return HIGH;
}


typedef unsigned char byte;


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


int countdown_base = 00*60+4;
volatile int countdown;
byte time_bcd[4];
/* unsigned long time = 0; */
bool pulse = 0;
bool mode = 0; // 0 = standart; 1 = edit
int submode = 3;


/* 7-Segment pin D4 J1 */
#define display_digit_0 23

/* 7-Segment pin D3 J4 */
#define display_digit_1 24

/* 7-Segment pin D2 J3 */
#define display_digit_2 25

/* 7-Segment pin D1 J2 */
#define display_digit_3 26


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
        pin_write(control_digit_pins[x], LOW);
    }
}

/* 74HC595 pin 12 STCP J_CLK1 (2) */
#define display_latch_pin 16

/* 74HC595 pin 10 MR SRCLR J_CLR1 (1) */
#define display_clock_pin 28

/* 74HC595 pin 14 DS J_SER1 (4) */
#define display_data_pin 27


void shift_out ( uint8_t val, uint8_t data_pin, uint8_t clock_pin ) {
    uint8_t i;
    for (i = 0; i < 8; i++)  {
        if (!!(val & (1 << (7 - i)))) {
            pin_write(data_pin, HIGH);
        } else {
            pin_write(data_pin, LOW);
        }
        pin_write(clock_pin, HIGH);
        pin_write(clock_pin, LOW);
   }
}


void DisplaySegments ( byte displayDigits[4] ) {
    for (int x=0; x<4; x++) { // for all four digit
        display_off();
        pin_write(display_latch_pin, LOW);  // [=latch down=]
        shift_out(displayDigits[x],
                  display_data_pin, display_clock_pin);
        pin_write(display_latch_pin, HIGH); // [=latch up=]
        if (pulse && mode) {
            if (submode == x) {
                pin_write(control_digit_pins[x], LOW); // turn off
            } else {
                pin_write(control_digit_pins[x], HIGH); // turn on
            }
        } else {
            pin_write(control_digit_pins[x], HIGH); // turn on
        }
        /* _delay_ms(1); */
    }
    display_off();
}


void setup () {
    DDRC = 0b111111;
    DDRB = 0b11111111;
    DDRD = 0b11111111;

    /* Допустим таймер в режиме по переполнению */
    /* и контроллер запущен с тактовой частотой 1 МГц. */

    /* Если таймер 16-битный, он может считать до */
    /* максимального значения (2^16 – 1), или 65535. */

    /* При 1 МГц цикл выполняется 1/(1 * 10^6) секунды */

    /* Это означает что 65535 отсчетов произойдут за */
    /* 0.065535 сек. */

    /* Можно использовать делитель, который позволяет  */
    /* поделить тактовый сигнал на степень двойки. */

    /* В регистре TCCR1B есть три бита CS устанавливающие */
    /* наиболее подходящее разрешение. */
    /* Если установить биты CS10 и CS12 используя: */
    /* TCCR1B |= (1 << CS10); */
    /* TCCR1B |= (1 << CS12); */
    /* то делитель будет установлен на 1024. */

    /* Это дает разрешение */
    /* таймера 1/(1 ∗ 10^6 / 1024) или 0.001024 с. */
    /* Теперь таймер будет переполняться */
    /* каждые 0.001024*65535 = 67.10784 c */

    /* Но есть и другой режим - сброс по совпадению (CTC). */

    /* Используя режим CTC надо понять, сколько циклов  */
    /* нужно, чтобы получить интервал в одну секунду.  */
    /* Если коэффициент деления по-прежнему равен 1024 */

    /* Расчет будет следующий: */
    /* (target_time) = (timer_resolution) * (timer_cnts + 1) */
    /* (timer_cnts + 1) = (target_time) / (timer_resolution) */
    /* (timer_cnts + 1) = (1 s) / (0.001024 s) */
    /* (timer_cnts + 1) = 976.5625 */
    /* (timer_cnts) = 976.5625 - 1 = 975.5625 */

    /* Нужно добавить дополнительную единицу к числу */
    /* отсчетов, т.к. в CTC при совпадении счетчика */
    /* с регистром A он сбросит отсчет в ноль. Сброс */
    /* занимает один такт: */
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

    /* Настройка Timer1 для отсчета секунд */
    /* отключить глобальные прерывания */
    cli();
    /* установить регистр TCCR1A в 0 */
    TCCR1A = 0;
    /* установка регистров совпадения */
    OCR1A = 975;
    /* включение Timer1 в CTC режим */
    /* остальные биты равны нулю */
    TCCR1B =
        (1 << WGM12) | /* Режим CTC, очистка после совпадения */
        (1 << CS10 ) | /* коэффициент деления 1024 */
        (1 << CS12 );
    /* включение прерывания по совпадению */
    TIMSK1 |= (1 << OCIE1A) ;
    /* включить глобальные прерывания */
    sei();

    /* Настройка Timer2 для сканирования клавиатуры */
    /* отключить глобальные прерывания */
    cli();
    /* Очистка Timer2 при совпадении */
    TCCR2A = 0;
    /* установка регистров совпадения */
    OCR2A = 1;
    /* включение Timer2 в CTC режим */
    /* остальные биты равны нулю */
    TCCR2B =
        (1 << WGM12) | /* Режим CTC, очистка после совпадения */
        (1 << CS10 ) | /* коэффициент деления 1024 */
        (1 << CS12 );
    /* включение прерывания по совпадению */
    TIMSK2 |= (1 << OCIE2A) ;
    /* включить глобальные прерывания */
    sei();
}


void int_to_time_bcd ( int param, byte result[4] ) {
    int minutes = param / 60;
    int seconds = param % 60;
    int minute_hi = minutes / 10;
    int minute_lo = minutes % 10;
    int second_hi = seconds / 10;
    int second_lo = seconds % 10;
    result[0] = second_lo;
    result[1] = second_hi;
    result[2] = minute_lo;
    result[3] = minute_hi;
}


int time_bcd_to_int ( byte param[4] ) {
    int minute_hi = param[3];
    int minute_lo = param[2];
    int second_hi = param[1];
    int second_lo = param[0];
    int minutes = minute_hi * 10 + minute_lo;
    int seconds = second_hi * 10 + second_lo;
    return minutes * 60 + seconds;
}


void time_bcd_to_time_str ( byte param[4], byte result[4] ) {
    result[0] = table[param[0]];
    result[1] = table[param[1]];
    result[2] = table[param[2]];
    result[3] = table[param[3]];
}


#define rows_cnt 4
#define cols_cnt 6


/* TODO: dbg */
int pinIn  [rows_cnt] = {5,4,3,2};        // входы клавиатуры
int pinOut [cols_cnt] = {6,7,8,9,10,11};  // выходы клавиатуры


const char value[rows_cnt][cols_cnt] =
{
 {'7', '8', '9', '/', '%', 'C'},
 {'4', '5', '6', '*', '^', 'X'},
 {'1', '2', '3', '-', 'X', '_'},
 {'X', '0', ',', '+', 'X', '='}
};


#define keyb_data_pin  12
#define keyb_clock_pin 10
#define keyb_latch_pin 11

void clear_shift_register () {
    pin_write(keyb_latch_pin, LOW);
    shift_out(0b11111111, keyb_data_pin, keyb_clock_pin);
    pin_write(keyb_latch_pin, HIGH);
}

char keyboard_scan () {
    clear_shift_register();
    /* То, что мы и за символ не считаем */
    char result = 'X';
    /* цикл, передающий LOW по каждому столбцу */
    for (int i=cols_cnt-1; i>=0; i--) {
        pin_write(keyb_latch_pin, LOW);
        shift_out(~(1<<i), keyb_clock_pin, keyb_data_pin);
        pin_write(keyb_latch_pin, HIGH);
        /* цикл, принимающих LOW по строкам */
        for (int j=0; j<rows_cnt; j++)  {
            /* если один из указанных портов входа равен LOW, то.. */
            if (pin_read(pinIn[j]) == LOW) {
                result = value[j][i];
            }
        }
    }
    clear_shift_register();
    return result;
}


bool need_keyb_scan_flag = false;


int main () {

    /* Setup */
    setup();

    /* Greeting */
    byte displayDigits[] = { 0b00111111, 0b00111000,
                             0b01110111, 0b01110110 };
    countdown = 200;
    while ( countdown > 199 ) {
        DisplaySegments( displayDigits );
    }
    display_off();

    /* Setup time settings */
    countdown = countdown_base;

    /* Main Loop */
    while(1) {

        /* time_bcd is a countdown */
        int_to_time_bcd( countdown, time_bcd );
        /* displayDigits is a time_bcd */
        time_bcd_to_time_str( time_bcd, displayDigits );
        /* show DisplayDigits */
        DisplaySegments( displayDigits );

        /* keyboard scan */
        if ( need_keyb_scan_flag ) {
            pulse = !pulse;
            char symbol = keyboard_scan();
            /* ... TODO ... */
            need_keyb_scan_flag = false;
        }

        /* Test for pin_read */
        if ( 0 == pin_read(2) ) {
            pin_write(15, LOW);
        } else {
            pin_write(15, HIGH);
        }

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

/* Прерывание по переполнению таймера 2 */
ISR(TIMER2_COMPA_vect)
{
    need_keyb_scan_flag = true;
}
