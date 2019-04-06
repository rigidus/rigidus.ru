const int btn_1         = 8;
const int relay_1       = 4;

boolean btn_released = true;
boolean state = LOW;
long duration = 0;
const long period = 4000;
const long interval = 1000;
unsigned long prev_mils = 0;
boolean prev_latch = LOW;
boolean latch = LOW;

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
        if (btn_released) {
            btn_released = false;
            delay(100);
            Serial.println("btn_pressed");
            if (HIGH == state) {
                Serial.println("switch off, state = LOW");
                state = LOW;
                duration = 0;
            } else {
                Serial.println("switch on, state = HIGH");
                state = HIGH;
                duration = period;
            }
        }
        
    } else {
        if (!btn_released) {
            Serial.println("btn_released");
            delay(100);
            btn_released = true;
            
        }
        if (state == HIGH) {
            if (cur_mils < prev_mils) {
                prev_mils = cur_mils;
            } else {
                unsigned long last_time = cur_mils - prev_mils;
                if ( last_time >= interval ) {
                    Serial.print("last_time = "); Serial.println(last_time);
                    prev_mils = cur_mils;
                    long decremented = duration - interval;
                    if ( decremented < 0 ) {
                        state = LOW;
                        Serial.println("millis() >= duration (is over);\n state = LOW;");
                    } else {
                        duration = decremented;
                        Serial.print("duration = ");
                        Serial.println(decremented);
                    }
                }
            }
        }
    }
    
    latch = state;
    if ( latch != prev_latch ) {
        prev_latch = latch;
        digitalWrite(relay_1, latch);
        Serial.print("=> "); Serial.println(latch);
    }
}
