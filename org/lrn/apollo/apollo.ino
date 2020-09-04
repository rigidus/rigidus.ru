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
{ {' ', '0', ',', '+', '*', '='},
  {'1', '2', '3', '-', '1', '~'},
  {'4', '5', '6', '*', '^', ' '},
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

/* output_method: */
/*     the four least significant bits controls data handling, */
/*     each bit controls associated digit starting with least-significant bit 0, */
/*     i.e. B1010, digit1 & digit3 are raw, digit0 & digit2 use table array */
/*   1 = raw byte */
/*   0 = table array index */
const byte base_output_method = 0b1000;
byte output_method = base_output_method;

/* tracks time */
unsigned long onTime = 0;

/* switch between HexCounter (table array) and RawDisplay (raw bytes) */
/*    false = HexCounter */
/*     true = RawDisplay */
bool switchView = false;

/* RawDisplay counter */
unsigned int counter = 0;

int digitDelay = 50;                  // delay between incrementing digits (ms)
int brightness = 90;                  // valid range of 0-100, 100=brightest
unsigned int ShowSegCount = 250;      // number of RawDisplay loops before switching again

/* ***************************************************
 *                   Functions                       *
 *************************************************** */

void matrix ()
{
  for (int i = 0; i < rows_cnt; i++) // цикл, передающий 0 по всем столбцам
  {
    digitalWrite(PinOut[i], LOW);
    //Serial.print(i);
    //Serial.println("");
    for (int j = 0; j < cols_cnt; j++) // цикл, принимающих 0 по строкам
    {
      //Serial.print(" ");
      //Serial.print(j);
      //Serial.print(" out:");
      //Serial.print(PinOut[i]);
      //Serial.print(" in:");
      //Serial.print(PinIn[j]);
      if (digitalRead(PinIn[j]) == LOW) // если один из указанных портов входа равен 0, то..
      {
        //Serial.print(" ["); Serial.print(i); Serial.print(" "); Serial.print(j); Serial.print("]");
        Serial.println(value[i][j]);
        delay(200);
      }
      //Serial.println("");
      //delay(200);
    }
    digitalWrite(PinOut[i], HIGH);
  }
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

/* void HexCounter() { */
/*     /\* Increments values stored in displayDigits array to */
/*      * creates a Hex counter from the table array. */
/*      * Uses mixed display types: */
/*      *    Digit3 | Digit2 | Digit1 | Digit0 */
/*      *    --------------------------------- */
/*      *       C   |   0    |   0    |    0 */
/*      *\/ */

/*     //increment values for digits 0-2 */
/*     bool incrementValue = true; */
/*     for (int d = 0; d < 3; d++){ */
/*         int x = int(displayDigits[d]); */
/*         if (incrementValue == true) { */
/*             x++; */
/*             incrementValue = false; */
/*             if (x > 15) { */
/*                 displayDigits[d] = 0; */
/*                 incrementValue = true; */
/*             } else { */
/*                 displayDigits[d] = byte(x); */
/*             } */
/*         } */
/*     } */

/*     // Set digit3 value */
/*     displayDigits[3] = 0b01001001; */
/*     // Set digitSwitch option */
/*     output_method = 0b1000; */

/*     if ( (displayDigits[0] == 0) && */
/*          (displayDigits[1] == 0) && */
/*          (displayDigits[2] == 0) ) { */
/*         switchView = !switchView; */
/*         // Reset array */
/*         for (int x = 0; x < 5; x++) { */
/*           displayDigits[x]=0; */
/*         } */
/*         output_method = 0b0000; */
/*     } */
/* } */

/* void RawDisplay(){ */

/*     // HALO */
/*     displayDigits[0] = 0b00111111;  // 0 */
/*     displayDigits[1] = 0b00111000;  // L */
/*     displayDigits[2] = 0b01110111;  // A */
/*     displayDigits[3] = 0b01110110;  // H */

/*      // Set digitSwitch option */
/*     output_method = 0b1111; */

/*     if (counter < ShowSegCount){ */
/*         counter++; */
/*     } else { */
/*         // Reset everything */
/*         counter = 0; */
/*         switchView = !switchView; */
/*         // Reset array */
/*         for (int x =0; x<5; x++) { */
/*             displayDigits[x]=0; */
/*         } */
/*         output_method = base_output_method; */
/*     } */
/* } */

void hex2disp (int param, byte result[4]) {
    int temp = param;
    int mask = 0xF;
    for (int x=0; x<4; x++) {
        result[x] = table[(temp & mask)];
        temp = temp >> 4;
    }
}

void loop() {

    byte displayDigits[] = { 0b00111111, 0b00111000,
                             0b01110111, 0b01110110 };
    hex2disp( 0xDEAD, displayDigits );
    DisplaySegments(displayDigits);

    /* *************************************
     *         Control Brightness          *
     * *********************************** */
    delayMicroseconds(1638*((100-brightness)/10));  // largest value 16383

    /* /\* ************************************* */
    /*  *        Selects Display Type         * */
    /*  * *********************************** *\/ */
    /* unsigned long nowValue = millis() - onTime; */
    /* if (nowValue >= long(digitDelay)){ */
    /*     onTime = millis(); */
    /*     if (switchView == true) { */
    /*       RawDisplay(); */
    /*     } else { */
    /*       HexCounter(); */
    /*     } */
    /* } */
}

void setup() {
  pinMode(latchPin,OUTPUT);
  pinMode(clockPin,OUTPUT);
  pinMode(dataPin,OUTPUT);
  for (int x=0; x<4; x++){
    pinMode(control_digit_pins[x],OUTPUT);
    digitalWrite(control_digit_pins[x],LOW);  // Turns off the digit
  }

  for (int i = 0; i < rows_cnt; i++) {
    // инициализируем порты на выход (подают нули на столбцы)
    pinMode (PinOut[i], OUTPUT);
  }
  for (int i = 0; i < cols_cnt; i++) {
    // инициализируем порты на вход с подтяжкой к плюсу (принимают нули на строках)
    pinMode (PinIn[i], INPUT);
    digitalWrite (PinIn[i], HIGH);
  }

  Serial.begin(9600);
}
