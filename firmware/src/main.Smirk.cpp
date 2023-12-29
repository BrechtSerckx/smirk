
// Boilerplate for injecting strings from macros
#define ST(A) #A
#define STR(A) ST(A)

#include <Arduino.h>
#include <HTTPClient.h>
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
  // if connection fails, it starts an access point with the specified name
  // then goes into a blocking loop awaiting configuration and will return success result

  bool res;
  res = wm.autoConnect(STR(AP_SSID),STR(AP_PASSWORD)); // password protected ap

  if(!res) {
    Serial.println("Failed to connect");
    // FIXME: what to do here?
    // ESP.restart();
  } 
  else {
    //if you get here you have connected to the WiFi    
    Serial.println("connected...yeey :)");
  }
}

void registerMaster() {
  HTTPClient http;

  Serial.print("[HTTP] begin...\n");
  // Split server address in two, as the preprocessor can't seem to handle `://`?
  http.begin(STR(SERVER_PROTOCOL) "://" STR(SERVER_ADDRESS) "/register");

  Serial.print("[HTTP] POST...\n");
  // start connection and send HTTP header
  http.addHeader("Content-Type", "application/json");
  int httpCode = http.POST("{ \"nodeId\" : \"" STR(NODE_ID) "\" }");

  // httpCode will be negative on error
  if(httpCode > 0) {
    // HTTP header has been send and Server response header has been handled
    Serial.printf("[HTTP] POST... code: %d\n", httpCode);

    // file found at server
    if(httpCode == HTTP_CODE_OK) {
      String payload = http.getString();
      Serial.println(payload);
    }
  } else {
    Serial.printf("[HTTP] POST... failed, error: %s\n", http.errorToString(httpCode).c_str());
  }

  http.end();
}

void setup() {
  setupSerial();
  setupWiFi();
  registerMaster();
}

void loop() {
}
