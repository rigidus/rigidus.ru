/* Four Digit Hex Counter */
/*    Uses: one 74HC595 shift register */
/*          one four digit seven segment display - common cathode (5461AS-1) */
/*          four 2N2222A transistors */

const int digit0   = 14;   // 7-Segment pin D4 J1
const int digit1   = 15;   // 7-Segment pin D3 J4
const int digit2   = 16;   // 7-Segment pin D2 J3
const int digit3   = 17;   // 7-Segment pin D1 J2
const int clockPin = 18;   // 74HC595 pin 10 MR SRCLR J_CLR1 (1)
const int dataPin  = 19;  // 74HC595 pin 14 DS J_SER1 (4)
const int latchPin = 13;  // 74HC595 pin 12 STCP J_CLK1 (2)

#define rows_cnt 4
#define cols_cnt 6

int PinOut[rows_cnt] {5,4,3,2}; // выходы
int PinIn [cols_cnt] {6,7,8,9,10,11}; // входы

const char value[rows_cnt][cols_cnt]
{ {'X', '0', ',', '+', '*', '='},
  {'1', '2', '3', '-', '1', '~'},
  {'4', '5', '6', '*', '^', 'B'},
  {'7', '8', '9', '/', '%', 'C'}
};

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

byte control_digit_pins[] = { digit0, digit1, digit2, digit3 };  // pins to turn off-&-on digits

char matrix ()
{
    char result = 'X';
    /* цикл, передающий 0 по всем столбцам */
    for (int i = 0; i < rows_cnt; i++) {
        digitalWrite(PinOut[i], LOW);
        /* Serial.print(i); */
        /* Serial.println(""); */
        /* цикл, принимающих 0 по строкам */
        for (int j = 0; j < cols_cnt; j++)  {
            /* Serial.print(" "); */
            /* Serial.print(j); */
            /* Serial.print(" out:"); */
            /* Serial.print(PinOut[i]); */
            /* Serial.print(" in:"); */
            /* Serial.print(PinIn[j]); */
            /* если один из указанных портов входа равен 0, то.. */
            if (digitalRead(PinIn[j]) == LOW) {
                /* Serial.print(" [ "); */
                /* Serial.print(i); */
                /* Serial.print(" "); */
                /* Serial.print(j); ; */
                result = value[i][j];
                /* Serial.print(value[i][j]); */
                /* Serial.print(" ]"); */
                /* Serial.println(" "); */
                /* delay(200); */
            }
            /* Serial.println(""); */
            /* delay(200); */
        }
        digitalWrite(PinOut[i], HIGH);
    }
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
        digitalWrite(control_digit_pins[x], HIGH); // turn on one digit
        delay(1); // 1 or 2 is ok
    }
    display_off();
}

void hex2disp (int param, byte result[4]) {
    int temp = param;
    int mask = 0xF;
    for (int x=0; x<4; x++) {
        result[x] = table[(temp & mask)];
        temp = temp >> 4;
    }
}

unsigned char hex2bcd (unsigned char x)
{
    unsigned char y;
    y = (x / 10) << 4;
    y = y | (x % 10);
    return (y);
}

void int_to_time_str (int param, byte result[4]) {
    int minutes = param / 60;
    int seconds = param % 60;
    int minute_hi = minutes / 10;
    int minute_lo = minutes % 10;
    int second_hi = seconds / 10;
    int second_lo = seconds % 10;
    result[0] = table[second_lo];
    result[1] = table[second_hi];
    result[2] = table[minute_lo];
    result[3] = table[minute_hi];
}

int countdown_base = 90*60+21;
volatile int countdown;
unsigned long time = 0;

void loop() {
    /* time cycle */
    unsigned long new_time = millis();
    if ((new_time > (time + 200))) {
        time = new_time;
        /* Serial.println(time); */
        char symbol = matrix();
        if ('X' != symbol) {
            Serial.println(symbol);
        }
    }
    byte displayDigits[] = { 0b00111111, 0b00111000,
                             0b01110111, 0b01110110 };
    //hex2disp( countdown, displayDigits );
    int_to_time_str( countdown, displayDigits );
    DisplaySegments(displayDigits);
}

void setup() {
  pinMode(latchPin,OUTPUT);
  pinMode(clockPin,OUTPUT);
  pinMode(dataPin,OUTPUT);
  for (int x=0; x<4; x++){
    pinMode(control_digit_pins[x],OUTPUT);
    digitalWrite(control_digit_pins[x],LOW);  // Turns off the digit
  }
  // инициализируем порты на выход (подают нули на столбцы)
  for (int i = 0; i < rows_cnt; i++) {
    pinMode (PinOut[i], OUTPUT);
  }
  // инициализируем порты на вход с подтяжкой к плюсу
  // (принимают нули на строках)
  for (int i = 0; i < cols_cnt; i++) {
    pinMode (PinIn[i], INPUT);
    digitalWrite (PinIn[i], HIGH);
  }
  // обратный отсчет
  countdown = countdown_base;
  // инициализация Timer1 на 1 сек
  cli(); // отключить глобальные прерывания
  TCCR1A = 0; // установить TCCR1A регистр в 0
  TCCR1B = 0;
  OCR1A = 15624; // установка регистра совпадения
  TCCR1B |= (1 << WGM12); // включение в CTC режим
  // Установка битов CS10 и CS12 на коэффициент деления 1024
  TCCR1B |= (1 << CS10);
  TCCR1B |= (1 << CS12);
  TIMSK1 |= (1 << OCIE1A);  // включение прерываний по совпадению
  sei(); // включить глобальные прерывания
  // Инициализация Serial
  Serial.begin(9600);
}

ISR(TIMER1_COMPA_vect)
{
    if (0 == countdown--) {
        countdown = countdown_base;
    }
}
