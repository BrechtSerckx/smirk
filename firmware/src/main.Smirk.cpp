
// Boilerplate for injecting strings from macros
 #define ST(A) #A
#define STR(A) ST(A)

#include <Arduino.h>
#include <WiFiManager.h>

#include <Smirk.h>

PrintLogger logger = PrintLogger(&Serial);
LogIRSender irSender = LogIRSender(&logger);
RawIRSignal mockIRSignal = RawIRSignal({}, 1000);
MockIRReceiver irReceiver = MockIRReceiver(&logger, mockIRSignal);

void setupSerial() {
  Serial.begin(SERIAL_BAUD_RATE);
}

void setupWiFi() {
  // explicitly set mode, esp defaults to STA+AP
  WiFi.mode(WIFI_STA);

  // WiFiManager, Local intialization. Once its business is done, there is no need to keep it around
  WiFiManager wm;

  // reset settings - wipe stored credentials for testing
  // these are stored by the esp library
  if(RESET_WIFI) {
    wm.resetSettings();
  }
  
  // Automatically connect using saved credentials,
  // if connection fails, it starts an access point with the specified name ( "AutoConnectAP"),
  // if empty will auto generate SSID, if password is blank it will be anonymous AP (wm.autoConnect())
  // then goes into a blocking loop awaiting configuration and will return success result

  bool res;
  // res = wm.autoConnect(); // auto generated AP name from chipid
  // res = wm.autoConnect("AutoConnectAP"); // anonymous ap
  res = wm.autoConnect(STR(AP_SSID),STR(AP_PASSWORD)); // password protected ap

  if(!res) {
    Serial.println("Failed to connect");
    // ESP.restart();
  } 
  else {
    //if you get here you have connected to the WiFi    
    Serial.println("connected...yeey :)");
  }
}

void setup() {
  setupSerial();
  setupWiFi();
}

void loop() {
}
