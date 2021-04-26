
// Code for calibrating soil moisture sensors measuring weight with load cells
// test version with upload to thingspeak

String ID = "dry_20Oct-2020"; // site ID and trial date

#include <Wire.h>

#include <Adafruit_ADS1015.h>     //Adafruit ADS1115 library-this controls the analog to digital converter      
  Adafruit_ADS1115 ads1115(0x48);     // sets I2C address for the ADC    

#include "FS.h"                   //File system library for SPIFFS

#include "uRTCLib.h"              //Library to run the RTC alarm -- there are other libraries available but this seemed most straightforward for just setting the alarm
#define URTCLIB_MODEL_DS3231 2        //RTC model
uRTCLib rtc(0x68);                    //RTC I2C address

// HX711 circuit wiring
#include "HX711.h"
const int LOADCELL_DOUT_PIN = 14; // pin d6 to dat
const int LOADCELL_SCK_PIN = 12; // pin d5 to clk

const int calib_factor = 447; // based on calibration using BX711calib_good sketch
const int zero_factor = 115077; // from same sketch

HX711 scale;

// onewire libraries for soil temp
#include <OneWire.h>              
#include <DallasTemperature.h>    //Both of these libraries are for the soil temp sensor
#define ONE_WIRE_BUS 2                // DS18B20 does not use I2C, runs on OneWire protocol
OneWire oneWire(ONE_WIRE_BUS);        //Initializes the OneWire protocol
DallasTemperature temps(&oneWire);

// libraries and setup for upload to thingspeak - based on https://github.com/mathworks/thingspeak-arduino
#include "ThingSpeak.h"
#include <ESP8266WiFi.h>

char ssid[] = "AirOrangeGuest";             // your network SSID (name) 
char pass[] = "";         // your network password
unsigned long myChannelNumber = 1115554;  // Replace the 0 with your channel number
const char * myWriteAPIKey = "58LMMCO8GXA8ZB0A";    // Paste your ThingSpeak Write API Key between the quotes 
WiFiClient  client;


void setup() {
  Serial.begin(9600);
  SPIFFS.begin();
  Wire.begin();
  digitalWrite( SDA, LOW);      //This may not be necessary but could recover I2C if shut down incorrectly
  digitalWrite( SCL, LOW);

  //Initialize wifi and thingspeak connection

    WiFi.begin(ssid, pass);
    WiFi.mode(WIFI_STA);
    ThingSpeak.begin(client);

  //ds3231 clock
    rtc.set_model(URTCLIB_MODEL_DS3231);
    rtc.set_rtc_address(0x68);
    rtc.alarmSet(URTCLIB_ALARM_TYPE_1_FIXED_S, 0, 0, 0, 1); // this sets the alarm to turn the system on and run code every hour on the minute
        // other alarm types are  URTCLIB_ALARM_TYPE_1_ALL_S - Every second
                                //URTCLIB_ALARM_TYPE_1_FIXED_S - Every minute at given second
                                //URTCLIB_ALARM_TYPE_1_FIXED_MS - Every hour at given Minute:Second
                                //URTCLIB_ALARM_TYPE_1_FIXED_HMS - Every day at given Hour:Minute:Second
                                //URTCLIB_ALARM_TYPE_1_FIXED_DHMS - Every month at given DAY-Hour:Minute:Second
                                //URTCLIB_ALARM_TYPE_1_FIXED_WHMS - Every week at given DOW + Hour:Minute:Second
                                //URTCLIB_ALARM_TYPE_2_ALL_M - Every minute at 00 Seconds
                                //URTCLIB_ALARM_TYPE_2_FIXED_M - Every hour at given Minute(:00)
                                //URTCLIB_ALARM_TYPE_2_FIXED_HM - Every day at given Hour:Minute(:00)
                                //URTCLIB_ALARM_TYPE_2_FIXED_DHM - Every month at given DAY-Hour:Minute(:00)
                                //URTCLIB_ALARM_TYPE_2_FIXED_WHM - Every week at given DOW + Hour:Minute(:00)
        // for _FIXED_ alarms, the integers set the time of the alarm -- second, minute, hour, dayofweek
            //so for an alarm every hour at :30, (URTCLIB_ALARM_TYPE_1_FIXED_MS, 0, 30, 0, 1)
        // the library uses 1 as default for dayofweek so I haven't changed that but it doesn't affect most alarms
        // should be possible to alternate alarms 1 and 2 for more intervals but I haven't gotten that working

  //ads1115 analog to digital converter for vmc
    ads1115.begin();    
    ads1115.setGain(GAIN_ONE); //set gain on the ADC

   //OneWire for soil temp probe
    temps.begin();

  // hx711 for load cell
    scale.begin(LOADCELL_DOUT_PIN, LOADCELL_SCK_PIN);
    scale.set_scale(calib_factor); // calibration to read weight in g
    scale.set_offset(zero_factor); // tare wt for scale setup
 
  //Initialize SPIFFS
  File f = SPIFFS.open("/data.csv", "a"); // open SPIFFS file in annotate mode to add one line of data at the end



  
  // collect sensor data
      rtc.refresh(); //update time from the rtc
   
      int y  = rtc.year();
      int mo = rtc.month();
      int d  = rtc.day();
      int h  = rtc.hour();
      int m  = rtc.minute();

      int raw_vmc = ads1115.readADC_SingleEnded(0); // soil resistance

      float load = scale.read_average(20); // load cell reading 
      float raw_wt_g = scale.get_units(20);

      temps.requestTemperatures();
      temps.getTempCByIndex(0);  // soil temp

      delay(800); // soil temp requires delay and re-read

      float soiltemp = temps.getTempCByIndex(0); // this is the reading that is actually written

      int power = analogRead(A0); //this reads the 5V line through the resistor to A0

  // write data to Thingspeak



    int attempt = 1 ;
    while(attempt<20 & WiFi.status() != WL_CONNECTED) {
        Serial.print("Attempt #: ");
        Serial.println(attempt);
        delay(500);     
        attempt += 1;
    } 

    if(WiFi.status() != WL_CONNECTED){
      f.println("Err5: Could not connect");
    }
    if(WiFi.status() == WL_CONNECTED){
       ThingSpeak.setField(1,ID);
       ThingSpeak.setField(2,h);
       ThingSpeak.setField(3,m);
       ThingSpeak.setField(4,raw_vmc);
       ThingSpeak.setField(5,load);
       ThingSpeak.setField(6,raw_wt_g);
       ThingSpeak.setField(7,soiltemp);
       ThingSpeak.setField(8,power);
        
      int x = ThingSpeak.writeFields(myChannelNumber,myWriteAPIKey);

      if(x != 200){
        delay(500);
        x = ThingSpeak.writeFields(myChannelNumber,myWriteAPIKey);
      }

      if(x != 200){
        f.print("Err: ");
        f.println(x);
      }
    }

    delay(500);


  // write sensor data to file
      f.print(y);
      f.print(",");
      f.print(mo);
      f.print(",");
      f.print(d);
      f.print(",");
      f.print(h);
      f.print(",");
      f.print(m);
      f.print(",");
      f.print(raw_vmc);
      f.print(",");
      f.print(load);
      f.print(",");
      f.print(raw_wt_g);
      f.print(",");
      f.print(soiltemp);
      f.print(",");
      f.print(power);
      f.print(",");
      f.print(ID);
      f.println();
      
  
  // close file
      f.close();        // Data is not actually written to file until this runs


  

    
    
  // reset clock flag to turn power off
      rtc.alarmClearFlag(URTCLIB_ALARM_1);

  // if the alarm doesn't work to turn off    
  
  f = SPIFFS.open("/data.csv", "a");
  f.println("err1");
  f.close();                                    // Err1 = ESP did not turn off after clearing the alarm flag. 
                                                    //This will appear once in data when ESP is plugged in because it cannot power off
  
  //delay(60*1000);


  f = SPIFFS.open("/data.csv", "a");  // errors after this should never appear unless there is something really wrong
  f.println("err2");                  // different numbers to allow diagnosis of where the ESP got stuck
  f.close();

}


void loop() {
   // testing download data all-in-one script

  File f = SPIFFS.open("/data.csv", "a");
  f.println("loop");
  f.close();

  if(Serial) {
    File f = SPIFFS.open("/data.csv", "a");  //open in annotate mode to see how long it is - file name must match exactly
    if (!f) {
        Serial.println("file open failed");
      };
  
  

    f.println();
    float len = f.position(); //this determines the number of bytes in the file
    //Serial.print(len);
  
    f.close();
  
    delay(2000);
  
    // open file for reading
    f = SPIFFS.open("/data.csv", "r"); //open file in read mode to print the data to the serial monitor
    if (!f) {
          Serial.println("file open failed");
      }  
    
    // read file

    Serial.println();
  
    for (int i=1; i<=len; i++){ 
      //this currently will keep printing a huge number of lines to the serial output
      //if you know how many bytes each line will be, change line above to read ... ; i<=len/[bytesperline];i++ ...
      //alternatively, just close the puTTY connection after it has stopped reading in data
      String s=f.readStringUntil('\n'); //reads the first line of the file 
      Serial.println(s);
    }
  }
  
  ESP.restart();
  delay(60*1000);

  f = SPIFFS.open("/data.csv","a");
  f.println("err4");
  f.close();

  ESP.restart();
}
