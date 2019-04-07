const int btn_1 = 8;
const int relay_1 = 4;

boolean btn_1_released = true;
boolean state_1 = LOW;
long duration_1 = 0;
const long period_1 = 4000;
const long interval = 1000;
unsigned long prev_mils = 0;
boolean prev_latch_1 = LOW;
boolean latch_1 = LOW;

void setup()
{
    Serial.begin(9600);
    pinMode(relay_1, OUTPUT);
    pinMode(btn_1,INPUT);
}

void loop()
{
    unsigned long cur_mils = millis();
    
    if (HIGH == digitalRead(btn_1)) {
        if (btn_1_released) {
            btn_1_released = false;
            delay(100);
            Serial.println("btn_pressed");
            if (HIGH == state_1) {
                Serial.println("switch off, state_1 = LOW");
                state_1 = LOW;
                duration_1 = 0;
            } else {
                Serial.println("switch on, state_1 = HIGH");
                state_1 = HIGH;
                duration_1 = period_1;
            }
        }
        
    } else {
        if (!btn_1_released) {
            Serial.println("btn_1_released");
            delay(100);
            btn_1_released = true;
            
        }
        if (state_1 == HIGH) {
            if (cur_mils < prev_mils) {
                prev_mils = cur_mils;
            } else {
                unsigned long last_time = cur_mils - prev_mils;
                if ( last_time >= interval ) {
                    Serial.print("last_time = "); Serial.println(last_time);
                    prev_mils = cur_mils;
                    long decremented = duration_1 - interval;
                    if ( decremented < 0 ) {
                        state_1 = LOW;
                        Serial.println("millis() >= duration_1 (is over);\n state_1 = LOW;");
                    } else {
                        duration_1 = decremented;
                        Serial.print("duration_1 = ");
                        Serial.println(decremented);
                    }
                }
            }
        }
    }
    
    latch_1 = state_1;
    if ( latch_1 != prev_latch_1 ) {
        prev_latch_1 = latch_1;
        digitalWrite(relay_1, latch_1);
        Serial.print("=> "); Serial.println(latch_1);
    }
}
