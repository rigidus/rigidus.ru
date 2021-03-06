/* Four Digit Hex Counter */
/*    Uses: one 74HC595 shift register */
/*          one four digit seven segment display - common cathode (5461AS-1) */
/*          four 2N2222A transistors */

const int digit0   = 14;   // 7-Segment pin D4 J1
const int digit1   = 15;   // 7-Segment pin D3 J4
const int digit2   = 16;   // 7-Segment pin D2 J3
const int digit3   = 17;   // 7-Segment pin D1 J2
byte control_digit_pins[] = { digit0, digit1, digit2, digit3 };
const int clockPin = 18;   // 74HC595 pin 10 MR SRCLR J_CLR1 (1)
const int dataPin  = 19;  // 74HC595 pin 14 DS J_SER1 (4)
const int latchPin = 13;  // 74HC595 pin 12 STCP J_CLK1 (2)

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

int pinIn  [rows_cnt] {5,4,3,2};        // входы клавиатуры
int pinOut [cols_cnt] {6,7,8,9,10,11};  // выходы клавиатуры

const char value[rows_cnt][cols_cnt] {
    {'7', '8', '9', '/', '%', 'C'},
    {'4', '5', '6', '*', '^', 'X'},
    {'1', '2', '3', '-', 'X', '_'},
    {'X', '0', ',', '+', 'X', '='}
};


const int dataPinKeyb  = 12;
const int clockPinKeyb = 10;
const int latchPinKeyb = 11;

void printBinary(byte inByte) {
    for (int b = 7; b >= 0; b--) {  Serial.print(bitRead(inByte, b)); }
}

void clear_shift_register() {
    digitalWrite(latchPinKeyb, LOW);
    shiftOut(dataPinKeyb, clockPinKeyb, MSBFIRST, 0b11111111);
    digitalWrite(latchPinKeyb, HIGH);
}

char matrix ()
{
    clear_shift_register();
    /* То, что мы и за символ не считаем */
    char result = 'X';
    /* цикл, передающий LOW по каждому столбцу */
    for (int i=cols_cnt-1; i>=0; i--) {
        digitalWrite(latchPinKeyb, LOW);
        shiftOut(dataPinKeyb, clockPinKeyb, MSBFIRST, ~(1<<i));
        digitalWrite(latchPinKeyb, HIGH);
        /* цикл, принимающих LOW по строкам */
        for (int j=0; j<rows_cnt; j++)  {
            /* если один из указанных портов входа равен LOW, то.. */
            if (digitalRead(pinIn[j]) == LOW) {
                /* Serial.print(" [i=");  Serial.print(i); */
                /* Serial.print(" j=");   Serial.print(j); */
                /* Serial.print(" out:"); printBinary(~(1<<i)); */
                /* Serial.print(" in:");  Serial.print(pinIn[j]); */
                /* Serial.print(" ]");    Serial.print(" "); */
                result = value[j][i];
                Serial.print(value[j][i]);
                delay(200);
                Serial.println(" ");
            }
        }
    }
    clear_shift_register();
    return result;
}


void display_off () { // turn off digits
    for (int j=0; j<4; j++) {
        digitalWrite(control_digit_pins[j], LOW);
    }
}

void DisplaySegments(byte displayDigits[4]) {
    for (int x=0; x<4; x++) { // for all four digit
        display_off();
        digitalWrite(latchPin,LOW);  // [=latch down=]
        shiftOut(dataPin, clockPin, MSBFIRST, (displayDigits[x]));
        digitalWrite(latchPin,HIGH); // [=latch up=]
        if (pulse && mode) {
            if (submode == x) {
                digitalWrite(control_digit_pins[x], LOW); // turn off
            } else {
                digitalWrite(control_digit_pins[x], HIGH); // turn on
            }
        } else {
            digitalWrite(control_digit_pins[x], HIGH); // turn on
        }
        delay(1); // 1 or 2 is ok
    }
    display_off();
}

/* void hex2disp (int param, byte result[4]) { */
/*     int temp = param; */
/*     int mask = 0xF; */
/*     for (int x=0; x<4; x++) { */
/*         result[x] = table[(temp & mask)]; */
/*         temp = temp >> 4; */
/*     } */
/* } */

/* unsigned char hex2bcd (unsigned char x) */
/* { */
/*     unsigned char y; */
/*     y = (x / 10) << 4; */
/*     y = y | (x % 10); */
/*     return (y); */
/* } */

int time_bcd_to_int (byte param[4]) {
    int minute_hi = param[3];
    int minute_lo = param[2];
    int second_hi = param[1];
    int second_lo = param[0];
    int minutes = minute_hi * 10 + minute_lo;
    int seconds = second_hi * 10 + second_lo;
    return minutes * 60 + seconds;
}

void int_to_time_bcd (int param, byte result[4]) {
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

void time_bcd_to_time_str (byte param[4], byte result[4]) {
    result[0] = table[param[0]];
    result[1] = table[param[1]];
    result[2] = table[param[2]];
    result[3] = table[param[3]];
}


void submode_inc() {
    submode++;
    if (submode > 3) { submode = 3; }
}


void submode_dec() {
    submode--;
    if (submode < 0) { submode = 0; }
}


/* MAIN LOOP */
void loop() {
    /* time cycle */
    unsigned long new_time = millis();
    if ((new_time > (time + 200))) {
        pulse = !pulse;
        time = new_time;
        /* Serial.println(time); */
        char symbol = matrix();
        if ('X' != symbol) {
            Serial.println(symbol);
        }
        byte input = 0xF;
        switch (symbol) {
        case 'C':
            mode = 1;
            submode = 3;
            break;
        case '=':
            mode = 0;
            break;
        case '-':
            submode_inc();
            break;
        case '+':
            submode_dec();
            break;
        case '0':
            input = 0;
            break;
        case '1':
            input = 1;
            break;
        case '2':
            input = 2;
            break;
        case '3':
            input = 3;
            break;
        case '4':
            input = 4;
            break;
        case '5':
            input = 5;
            break;
        case '6':
            input = 6;
            break;
        case '7':
            input = 7;
            break;
        case '8':
            input = 8;
            break;
        case '9':
            input = 9;
            break;
        }
        if (mode && (input != 0xF)) {
            time_bcd[submode] = input;
            submode_dec(); // reverse becouse shematic
            countdown = time_bcd_to_int( time_bcd );
        }
    }
    byte displayDigits[] = { 0b00111111, 0b00111000,
                             0b01110111, 0b01110110 };
    /* time_bcd is a countdown */
    int_to_time_bcd( countdown, time_bcd );
    /* displayDigits is a time_bcd */
    time_bcd_to_time_str( time_bcd, displayDigits );
    /* show DisplayDigits */
    DisplaySegments( displayDigits );

    /* countdown = time_bcd_to_int( time_bcd ); */
}

void setup() {

    /* Инициализация семисегментного дисплея */
    pinMode(latchPin,OUTPUT);
    pinMode(clockPin,OUTPUT);
    pinMode(dataPin,OUTPUT);
    /* Turns off the digit */
    for (int x=0; x<4; x++){
        pinMode(control_digit_pins[x],OUTPUT);
        digitalWrite(control_digit_pins[x],LOW);
    }

    /* Инициализация клавиатуры */
    pinMode(latchPinKeyb,OUTPUT);
    pinMode(clockPinKeyb,OUTPUT);
    pinMode(dataPinKeyb,OUTPUT);
    /* инициализируем порты на выход (подают нули на столбцы) */
    for (int i = 0; i < (sizeof(pinOut)/sizeof(pinOut[0])); i++) {
        pinMode (pinOut[i], OUTPUT);
    }
    /* инициализируем порты на вход с подтяжкой к плюсу */
    /* (принимают нули на строках) */
    for (int i = 0; i < (sizeof(pinIn)/sizeof(pinIn[0])); i++) {
        pinMode (pinIn[i], INPUT);
        digitalWrite (pinIn[i], HIGH);
    }
    /* Инициализация клавиатуры единицами */
    /* на старте, потому что "бегущий ноль"  */
    digitalWrite(latchPinKeyb, LOW);
    shiftOut(dataPinKeyb, clockPinKeyb, MSBFIRST, 0b11111111);
    digitalWrite(latchPinKeyb, HIGH);

    /* обратный отсчет */
    countdown = countdown_base;

    /* инициализация Timer1 на 1 сек */
    cli(); /* отключить глобальные прерывания/ */
    TCCR1A = 0; /* установить TCCR1A регистр в 0 */
    TCCR1B = 0;
    OCR1A = 15624; /* установка регистра совпадения */
    TCCR1B |= (1 << WGM12); /* включение в CTC режим */
    /* Установка битов CS10 и CS12 на коэффициент деления 1024 */
    TCCR1B |= (1 << CS10);
    TCCR1B |= (1 << CS12);
    TIMSK1 |= (1 << OCIE1A);  /* включение прерываний по совпадению */
    sei(); /* включить глобальные прерывания */

    /* Инициализация Serial */
    Serial.begin(9600);
}

/* Прерывание по переполнению таймера 1 */
ISR(TIMER1_COMPA_vect)
{
    if (!mode) {
        if (0 == countdown--) {
            countdown = countdown_base;
        }
    }
}
