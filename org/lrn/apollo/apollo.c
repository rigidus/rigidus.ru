#define F_CPU 1000000UL // 1 MHz

#include <avr/io.h>
#include <util/delay.h>
#include <stdbool.h>
#include <avr/interrupt.h>
#include <avr/eeprom.h>

#define DBG 0

#ifndef DBG
#define DBG 0
#endif

#if (DBG)
    #define DBGDISP(arg) display[1]=arg;
#else
    #define DBGDISP(arg)
#endif


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

    uint8_t bit  = (1<<mask);

    uint8_t oldSREG = SREG;

    cli();

    if (val == LOW) {
        *port_pnt &= ~bit;
    } else {
        *port_pnt |= bit;
    }

    SREG = oldSREG;
    return 0;
}


int8_t pin_read ( uint8_t pin ) {
    if (pin > sizeof( pin_to_port_and_mask )) return -1;

    uint8_t port = ( pin_to_port_and_mask[pin] >> 4) ;
    volatile uint8_t *port_pnt;

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

    if ( 0 == ( (*port_pnt) & (1<<mask) ) ) {
        return LOW;
    }
    return HIGH;
}


typedef unsigned char byte;


byte table[] =
    {
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


#define EDIT_MODE 0
#define COUNTDOWN_MODE 1
#define SIGNAL_MODE 2
volatile byte mode = EDIT_MODE;
/* current digit for edit mode */
int8_t  submode = 3;

volatile bool need_keyb_scan_flag = false;
volatile bool need_display_refresh_flag = false;

volatile byte display[4];

volatile uint16_t  countdown_base = 0*60+5;
volatile int16_t   countdown;

/* blinking state for editable digit */
volatile bool pulse = false;




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
    for (uint8_t x=0; x<4; x++) { // for all four digit
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


void show () {
    /**
     * GLOBAL PARAMETERS:
     * - byte display[4]
     * GLOBAL CONSTANTS:
     * - display_latch_pin
     * - display_data_pin
     * - byte control_digit_pins[4]
     */
    for (uint8_t x=0; x<4; x++) {
        /* для всех цифр */
        display_off();
        pin_write(display_latch_pin, LOW);
        shift_out(display[x],
                  display_data_pin, display_clock_pin);
        pin_write(display_latch_pin, HIGH);
        if (pulse && (EDIT_MODE == mode)) {
            if (submode == x) {
                /* выключить текущую цифру */
                pin_write(control_digit_pins[x], LOW);
            } else {
                /* включить текущую цифру */
                pin_write(control_digit_pins[x], HIGH);
            }
        } else {
            /* включить текущую цифру */
            pin_write(control_digit_pins[x], HIGH);
        }
        /* _delay_ms(1); */
    }
    display_off();
}


void word_to_bcd ( uint16_t param, byte result[4] ) {
    uint8_t minutes = param / 60;
    uint8_t seconds = param % 60;
    uint8_t minute_hi = minutes / 10;
    uint8_t minute_lo = minutes % 10;
    uint8_t second_hi = seconds / 10;
    uint8_t second_lo = seconds % 10;
    result[0] = second_lo;
    result[1] = second_hi;
    result[2] = minute_lo;
    result[3] = minute_hi;
}


uint16_t bcd_to_word ( byte param[4] ) {
    uint8_t minute_hi = param[3];
    uint8_t minute_lo = param[2];
    uint8_t second_hi = param[1];
    uint8_t second_lo = param[0];
    uint8_t minutes = minute_hi * 10 + minute_lo;
    uint8_t seconds = second_hi * 10 + second_lo;
    return minutes * 60 + seconds;
}


void bcd_to_str ( byte param[4], byte result[4] ) {
    result[0] = table[param[0]];
    result[1] = table[param[1]];
    result[2] = table[param[2]];
    result[3] = table[param[3]];
}


#define rows_cnt 4
#define cols_cnt 6

const char value[rows_cnt][cols_cnt] =
    {
     {'7', '8', '9', '/', '%', 'C'},
     {'4', '5', '6', '*', '^', 'X'},
     {'1', '2', '3', '-', 'X', '_'},
     {'X', '0', ',', '+', 'X', '='}
    };

#define keyb_clock_pin 15
#define keyb_latch_pin 14
#define keyb_data_pin  13

/* входы клавиатуры */
uint8_t pinIn [rows_cnt] = { 6, 5, 4, 3 };


void clear_shift_register () {
    pin_write(keyb_latch_pin, LOW);
    shift_out(0b11111111, keyb_data_pin, keyb_clock_pin);
    pin_write(keyb_latch_pin, HIGH);
}


char keyboard_scan () {
    /* То, что мы и за символ не считаем */
    char result = 'X';
    clear_shift_register();
    for ( int8_t col=0; col<8; col++ ) {
        pin_write( keyb_latch_pin, LOW );
        shift_out( ~(1<<col), keyb_data_pin, keyb_clock_pin );
        pin_write( keyb_latch_pin, HIGH );
        /* цикл, принимающих LOW по строкам */
        for ( int8_t row=0; row<rows_cnt; row++ )  {
            /* если один из портов входа = LOW, то.. */
            if ( pin_read( pinIn[row] ) == LOW ) {
                #if (DBG)
                    byte tmp_bcd[4];
                    byte tmp_display[4];
                    word_to_bcd( col, tmp_bcd );
                    bcd_to_str( tmp_bcd, tmp_display );
                    display[3] = tmp_display[0]; /* col */
                    word_to_bcd( row, tmp_bcd );
                    bcd_to_str( tmp_bcd, tmp_display );
                    display[2] = tmp_display[0]; /* row */
                #endif
                /* получение символа */
                /* коррекция -2 по схеме подключения */
                result = value[row][col-2];
            }
        }
        /* если мы здесь то нет нажатий в этом столбце */
    }
    if ('X' != result) {
        /* есть символ */
    } else {
        #if (DBG)
            display[3] = 0b10000000;
            display[2] = 0b10000000;
            display[1] = 0b10000000;
        #endif
    }
    return result;
}


void submode_inc() {
    submode++;
    if (submode > 3) { submode = 3; }
}


void submode_dec() {
    submode--;
    if (submode < 0) { submode = 0; }
}

#define eeprom_address 46

void keyboard_handler ( uint8_t symbol ) {
    /**
     * MODIFY GLOBAL VARIABLES:
     * - countdown
     * - mode
     * - submode
     */
    byte input = 'X';
    switch (symbol) {
    case 'C': DBGDISP(table[0xC]); mode = EDIT_MODE; submode = 3;
        break;
    case '=': DBGDISP(0b01000001);
        break;
    case '-': DBGDISP(0b01000000); submode_inc();
        break;
    case '+': DBGDISP(0b01110011); submode_dec();
        break;
    case '*': DBGDISP(0b01001001);
        cli();
        eeprom_write_word ((uint16_t*)eeprom_address, countdown);
        sei();
        break;
    case ',': DBGDISP(0b10000000);
        /* нельзя начинать счет с нуля, будет антипереполнение */
        if (0 == countdown) countdown = 1;
        /* переключаемся в счетный режим. */
        mode = COUNTDOWN_MODE;
        break;
    case '%': DBGDISP(0b00010010);
        break;
    case '/': DBGDISP(0b01010010);
        cli();
        /* прочитать из eeprom */
        countdown = eeprom_read_word ((uint16_t*)eeprom_address);
        sei();
        break;
    case '^': DBGDISP(0b00000001);
        break;
    case '_': DBGDISP(0b00001000);
        break;
    case '0': DBGDISP(table[0]); input = 0; break;
    case '1': DBGDISP(table[1]); input = 1; break;
    case '2': DBGDISP(table[2]); input = 2; break;
    case '3': DBGDISP(table[3]); input = 3; break;
    case '4': DBGDISP(table[4]); input = 4; break;
    case '5': DBGDISP(table[5]); input = 5; break;
    case '6': DBGDISP(table[6]); input = 6; break;
    case '7': DBGDISP(table[7]); input = 7; break;
    case '8': DBGDISP(table[8]); input = 8; break;
    case '9': DBGDISP(table[9]); input = 9; break;
    default: return;
    }
    if ((EDIT_MODE == mode) && (input != 'X')) {
        /* изменить countdown */
        byte tmp_bcd[4];
        word_to_bcd( countdown, tmp_bcd );
        tmp_bcd[submode] = input;
        countdown = bcd_to_word( tmp_bcd );
        /* перейти к следующей цифре */
        submode_dec();
    }
}


void setup ();


int main () {
    setup();

    while(1) {

        if ( need_display_refresh_flag ) {
            /* пересчитаем и обновим переменную дисплея */
            uint8_t tmp_bcd[4];
            uint8_t tmp_disp[4];
            word_to_bcd( countdown,  tmp_bcd  );
            bcd_to_str ( tmp_bcd,    tmp_disp );
            #if (0 == DBG)
                display[3] = tmp_disp[3];
                display[2] = tmp_disp[2];
                display[1] = tmp_disp[1];
            #endif
            display[0] = tmp_disp[0];
            /* отобразим переменную дисплея */
            show();
            /* сбросим флаг */
            need_display_refresh_flag = false;
        }

        /* keyboard scan */
        if ( need_keyb_scan_flag ) {
            keyboard_handler( keyboard_scan() );
            /* сбросим флаг */
            need_keyb_scan_flag = false;
        }


        switch ( mode ) {
        case EDIT_MODE:
            open();
            /* выключаем Relay_1 */
            pin_write(11, LOW);
            /* выключаем звук */
            TCCR0B &= ~(1<<WGM02);
            break;
        case COUNTDOWN_MODE:
            close();
            /* включаем Relay_1 */
            pin_write(11, HIGH);
            /* выключаем звук */
            TCCR0B &= ~(1<<WGM02);
            break;
        case SIGNAL_MODE:
            /* выключаем Relay_1 */
            pin_write(11, LOW);
            /* пищащий звук */
            if (pulse) {
                /* если WGM02=0, пин OC0A отключен */
                TCCR0B |= (1<<WGM02);
            } else {
                TCCR0B &= ~(1<<WGM02);
            }
            break;
        }
    }
    return 0;
}


volatile uint8_t int_cnt = 0;

/* Прерывание по совпадению таймера 2 */
ISR(TIMER2_COMPA_vect)
{
    /* инкремент счетчика */
    int_cnt++;

    /* пересчитаем и обновим переменную дисплея */
    need_display_refresh_flag = true;

    /* не пора ли просканировать клавиатуру? */
    if ( 0b11 == (int_cnt & 0b11) ) {
        need_keyb_scan_flag = true; /* пора - установим флаг */
        pulse = !pulse; /* мигание */
    }

    /* не пора ли уменьшить countdown? */
    if ( 32 == int_cnt ) {
        /* пора - уменьшаем, если мы в режиме счета */
        if (COUNTDOWN_MODE == mode) {
            countdown--;
            /* Если досчитали - переходим в SIGNAL_MODE */
            if (0 == countdown) {
                mode = SIGNAL_MODE;
            }
        }
        /* обнуляем int_cnt */
        int_cnt = 0;
    }
}


void setup () {
    DDRC  = 0b111111;
    DDRB  = 0b11111111;
    DDRD  = 0b11100001; /* exept keyb */
    PORTD = 0b00011110; /*  PULLUP */

    /* Set countdown */
    uint16_t eeprom;
    cli();
    eeprom = eeprom_read_word((uint16_t*)eeprom_address);
    sei();
    if (0xFFFF != eeprom) {
        countdown_base = eeprom;
    }
    countdown = countdown_base;

    /*
      (defun resolution (freq div)
      (/ 1 (/ (* freq (expt 10 6)) div)))

      (defun cycle-cnt (target-time resolution)
      (- (/ target-time resolution) 1))

      (cycle-cnt 1 (resolution 1 1024))
      => 15609/16 = 975.5625
    */


    /* Настройка Timer2 для сканирования клавиатуры */
    /* отключить глобальные прерывания */
    cli();
    /* установить регистр TCCR2A в 0 */
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


    /* Чтобы издавать звуки, настроим Timer0 в режим CTC */
    /* отключить глобальные прерывания */
    cli();
    /* установить регистр TCCR0A с инверсией */
    /* пина OC0A, а пин OC0B не используется */
    TCCR0A =
        (1<<WGM00)  |
        (1<<WGM01)  |
        (1<<COM0A0) ;
    /* /\* установка регистра совпадения *\/ */
    OCR0A = 0x50;
    /* остальные биты равны нулю */
    TCCR0B =
        (0 << WGM02) | /* если WGM02=0, пин OC0A отключен */
        (1 << CS01 ) ; /* коэффициент деления 8 */
    /* включить глобальные прерывания */
    sei();


    /* Чтобы управлять сервой, настроим Timer1 в режим FastPWM */
    /* отключить глобальные прерывания */
    cli();
    /* каждые 20 микросекунд - OVF interrupt - TOP */
    ICR1 = 1000000 / 50 / 4 ;
    /* диапазон интервалов - 0,6 ms ... 2 ms */
    /* 1000000 Гц (частота) / 0 (предделитель)  = 125000 Гц */
    /* 1 / 125000 Гц = 0.000008s = 8 us (время инкремента на 1) */
    /* 0,6 ms = 600 us   / 8 = 75 */
    /* 2 ms   = 2000 us  / 8 = 250 */
    open();
    /* */
    TCCR1B |=
        (1<<ICES1) /* FastPWM */
        | (0<<CS10) | (1<<CS11) | (0<<CS12) /* предделитель = 8 */
        | (1<<WGM12) | (1<<WGM13); /* mode = PWM (ICR1) */
    TCCR1A |= (1<<WGM11); /* mode = PWM (ICR1) */
    TIMSK1 |= (1<<TOIE1) | (1<<OCIE1A); /* interrupts */
    sei();

}

void open () {
    OCR1A = 250;
}

void close () {
    OCR1A = 75;
}


ISR(TIMER1_OVF_vect)
{
    pin_write(9, HIGH);
}


ISR(TIMER1_COMPA_vect)
{
    pin_write(9, LOW);
}
