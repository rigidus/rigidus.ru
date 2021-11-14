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

void loop()
{
  matrix();
}

void setup()
{
  for (int i = 0; i < rows_cnt; i++) {
    // инициализируем порты на выход (подают нули на столбцы)
    pinMode (PinOut[i], OUTPUT);
  }
  for (int i = 0; i < cols_cnt; i++) {
    // инициализируем порты на вход с подтяжкой к плюсу (принимают нули на строках)
    pinMode (PinIn[i], INPUT);
    digitalWrite (PinIn[i], HIGH);
  }
  Serial.begin(9600); // открываем Serial порт
}
