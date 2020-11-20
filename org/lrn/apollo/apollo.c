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


/**
 * Выводит display[] на семисешментный индикатор
 * черехз регистр сдвига.
 * GLOBAL PARAMETERS:
 * - byte display[4]
 * GLOBAL CONSTANTS:
 * - display_latch_pin
 * - display_data_pin
 * - byte control_digit_pins[4]
 */
void show () {
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


#define rows_cnt 2
#define cols_cnt 8

const char value[rows_cnt][cols_cnt] =
    {
     {'4', '7', '5', '8', '6', '9', 'B', 'C'},
     {'1', '0', '2', 'F', '3', 'E', 'F', 'D'}
    };

#define keyb_clock_pin 15
#define keyb_latch_pin 14
#define keyb_data_pin  13

/* входы клавиатуры */
uint8_t pinIn [rows_cnt] = { 6, 5 };


void clear_shift_register () {
    pin_write(keyb_latch_pin, LOW);
    shift_out(0b11111111, keyb_data_pin, keyb_clock_pin);
    pin_write(keyb_latch_pin, HIGH);
}


/**
 * Работа с клавиатурой 8*2
 * с использованием 8-разрядного регистра сдвига
 * и кольцевого буфера
 */

#define  KEYB_RING_MAX 8
uint16_t keyb_ring [KEYB_RING_MAX] = {0,0,0,0,0,0,0,0};
uint8_t  keyb_ring_head = 0;
uint8_t  keyb_ring_tail = KEYB_RING_MAX-1;

/*
 * Возвращает инкремент индекса в кольцевом буфере
 */
uint8_t inc_keyb_ring_idx (uint8_t param) {
    uint8_t result = ++param;
    if ( result >= KEYB_RING_MAX ) {
        result = 0;
    }
    return result;
}

/* Сканирует клавиатуру и если состояние изменилось -
 * заносит изменившееся состояние в кольцевой буфер.
 * Если буфер переполнен - возвращает FALSE иначе TRUE
 */
bool keyb_scan () {
    /* Очищаем текущее состояние клавиатуры */
    uint16_t cur_keyb_state[rows_cnt] = {0,0};
    /* Очищаем регистр сдвига клавиатуры */
    clear_shift_register();
    /* Цикл опроса клавиатурной матрицы */
    for ( int8_t col=0; col<cols_cnt; col++ ) {
        /* бегущий по столбцам LOW */
        pin_write( keyb_latch_pin, LOW );
        shift_out( ~(1<<col), keyb_data_pin, keyb_clock_pin );
        pin_write( keyb_latch_pin, HIGH );
        /* цикл, принимающих LOW по строкам */
        for ( int8_t row=0; row<rows_cnt; row++ )  {
            /* если один из портов входа = LOW, то.. */
            if ( pin_read( pinIn[row] ) == LOW ) {
                /* Добавляем бит, если кнопка нажата */
                cur_keyb_state[row] = cur_keyb_state[row] | (1<<col);
                /* #if (DBG) */
                /*     display[0] = table[cur_keyb_state[0] & 0xF]; */
                /*     display[1] = table[(cur_keyb_state[0]>>4) & 0xF]; */
                /*     display[2] = table[cur_keyb_state[1] & 0xF]; */
                /*     display[3] = table[(cur_keyb_state[1]>>4) & 0xF]; */
                /* #endif */
            }
        }
    }
    /* Если состояние клавиатуры изменилось.. */
    uint16_t new_state = (cur_keyb_state[1] << 8) | cur_keyb_state[0];
    if (keyb_ring[keyb_ring_head] != new_state) {
        /* ..добавить его в кольцевой буфер */
        uint8_t new_head = inc_keyb_ring_idx(keyb_ring_head);
        if ( new_head == keyb_ring_tail) {
            /* буфер переполнен! */
            return false;
        } else {
            /* еще есть место */
            keyb_ring_head = new_head;
            keyb_ring[keyb_ring_head] = new_state;
        }
    }
    return true;
}

/*
 * Получает состояние клавиатуры из кольцевого буфера
 * Если буфер пуст - возвращает единицы во всех разрядах,
 * так как мы считаем, что на все кнопки одновременно
 * нажать невозможно.
 */
uint16_t get_keyb_ring () {
    if ( keyb_ring_tail == keyb_ring_head ) {
        /* буфер пуст */
        return 0xFFFF;
    } else {
        uint16_t state = keyb_ring[keyb_ring_tail];
        keyb_ring_tail = inc_keyb_ring_idx(keyb_ring_tail);
        return state;
    }
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

#define KEYCODE_1 0x0100
#define KEYCODE_2 0x0400
#define KEYCODE_3 0x1000
#define KEYCODE_4 0x0001
#define KEYCODE_5 0x0004
#define KEYCODE_6 0x0010
#define KEYCODE_7 0x0002
#define KEYCODE_8 0x0008
#define KEYCODE_9 0x0020
#define KEYCODE_0 0x0200
#define KEYCODE_A 0x0800
#define KEYCODE_B 0x2000
#define KEYCODE_C 0x4000
#define KEYCODE_D 0x0040
#define KEYCODE_E 0x0080
#define KEYCODE_F 0x8000

void keyboard_handler ( uint16_t keycode ) {
    /**
     * MODIFY GLOBAL VARIABLES:
     * - countdown
     * - mode
     * - submode
     */
    byte input = 'X';
    switch ( keycode ) {
    case KEYCODE_A:
        submode_inc();
        break;
    case KEYCODE_B:
        submode_dec();
        break;
    case KEYCODE_C:
        mode = EDIT_MODE; submode = 3;
        break;
    case KEYCODE_D:
        cli();
        eeprom_write_word ((uint16_t*)eeprom_address, countdown);
        sei();
        break;
    case KEYCODE_E:
        cli();
        countdown = eeprom_read_word ((uint16_t*)eeprom_address);
        sei();
        break;
    case KEYCODE_F:
        /* нельзя начинать счет с нуля, будет антипереполнение */
        if (0 == countdown) countdown = 1;
        /* переключаемся в счетный режим. */
        mode = COUNTDOWN_MODE;
        break;
    case KEYCODE_0: input = 0; break;
    case KEYCODE_1: input = 1; break;
    case KEYCODE_2: input = 2; break;
    case KEYCODE_3: input = 3; break;
    case KEYCODE_4: input = 4; break;
    case KEYCODE_5: input = 5; break;
    case KEYCODE_6: input = 6; break;
    case KEYCODE_7: input = 7; break;
    case KEYCODE_8: input = 8; break;
    case KEYCODE_9: input = 9; break;
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


void servo_on () {
    OCR1A = 250;
}


void servo_off () {
    OCR1A = 75;
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
                display[0] = tmp_disp[0];
            #endif
            /* отобразим переменную дисплея */
            /* display[3] = table[keyb_ring_head]; */
            /* display[2] = table[keyb_ring_tail]; */
            show();
            /* сбросим флаг */
            need_display_refresh_flag = false;
        }

        /* keyboard scan */
        if ( need_keyb_scan_flag ) {
            keyb_scan();
            uint16_t keyb_state = get_keyb_ring();
            if ( 0xFFFF == keyb_state ) {
                /* TODO: кольцевой буфер пуст */
            } else {
                if ( 0x0000 == keyb_state ) {
                    /* TODO: ничего не нажато */
                } else {
                    /* обработать нажатие */
                    keyboard_handler( keyb_state );
                }
            }
            /* сбросим флаг */
            need_keyb_scan_flag = false;
        }


        switch ( mode ) {
        case EDIT_MODE:
            servo_on();
            /* выключаем Relay_1 */
            pin_write(11, LOW);
            /* выключаем звук */
            TCCR0B &= ~(1<<WGM02);
            break;
        case COUNTDOWN_MODE:
            servo_off();
            /* включаем Relay_1 & Relay_2 */
            pin_write(10, HIGH);
            pin_write(11, HIGH);
            /* выключаем звук */
            TCCR0B &= ~(1<<WGM02);
            break;
        case SIGNAL_MODE:
            /* выключаем Relay_1 & Relay_2 */
            pin_write(10, LOW);
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
    servo_on();
    /* */
    TCCR1B |=
        (1<<ICES1) /* FastPWM */
        | (0<<CS10) | (1<<CS11) | (0<<CS12) /* предделитель = 8 */
        | (1<<WGM12) | (1<<WGM13); /* mode = PWM (ICR1) */
    TCCR1A |= (1<<WGM11); /* mode = PWM (ICR1) */
    TIMSK1 |= (1<<TOIE1) | (1<<OCIE1A); /* interrupts */
    sei();

}



ISR(TIMER1_OVF_vect)
{
    pin_write(9, HIGH);
}


ISR(TIMER1_COMPA_vect)
{
    pin_write(9, LOW);
}
