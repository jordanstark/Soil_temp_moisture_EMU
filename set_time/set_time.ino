// This script sets time on the EMU RTC without affecting SPIFFS memory

#include <Wire.h>
#include "RTClib.h" // this library is used to set the clock; the code here is based on its example sketch


RTC_DS3231 rtc;

char daysOfTheWeek[7][12] = {"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"};

void setup () {
  Serial.begin(9600);

  delay(3000); // wait for console opening

// FOR RTC
  #ifndef ESP8266
    while (!Serial); // for Leonardo/Micro/Zero
  #endif

  if (! rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }

    rtc.adjust(DateTime(F(__DATE__), F(__TIME__))); //this line sets the time--comment out if not needed 


    DateTime now = rtc.now(); //sets time in the ESP8266 to the time on the RTC
    
    Serial.print(now.year(), DEC); //prints the time -- this is useful even if not setting the time to check that RTC is functioning properly
    Serial.print('/');
    Serial.print(now.month(), DEC);
    Serial.print('/');
    Serial.print(now.day(), DEC);
    Serial.print(" (");
    Serial.print(daysOfTheWeek[now.dayOfTheWeek()]);
    Serial.print(") ");
    Serial.print(now.hour(), DEC);
    Serial.print(':');
    Serial.print(now.minute(), DEC);
    Serial.print(':');
    Serial.print(now.second(), DEC);
    Serial.println();


}

void loop () {
    
}
