
// Boilerplate for injecting strings from macros
#define ST(A) #A
#define STR(A) ST(A)

#include <Arduino.h>
#include <ESPmDNS.h>
#include <HTTPClient.h>
#include <WiFiManager.h>

#include <Smirk.h>

PrintLogger logger = PrintLogger(&Serial);
LogIRSender irSender = LogIRSender(&logger);
RawIRSignal mockIRSignal = RawIRSignal({}, 1000);
MockIRReceiver irReceiver = MockIRReceiver(&logger, mockIRSignal);

// WiFiManager
WiFiManager wm;
WiFiManagerParameter serverAddress("server_address", "Server Address", "", 50);

void setupSerial() {
  Serial.begin(SERIAL_BAUD_RATE);
}

void setupHostName() {
  String macAddress = WiFi.macAddress();
  Serial.print("MAC address: ");
  Serial.println(macAddress);
  macAddress.replace(":","");
  macAddress = macAddress.substring(6);
  const String hostname = "SmirkMate-" + macAddress;
  Serial.print("Setting hostname to: ");
  Serial.println(hostname);
  WiFi.setHostname(hostname.c_str());
}

void setupWiFi() {
  // explicitly set mode, esp defaults to STA+AP
  WiFi.mode(WIFI_STA);

  // reset settings - wipe stored credentials for testing
  // these are stored by the esp library
  if(RESET_WIFI) {
    wm.resetSettings();
  }

  // Invert theme, dark
  wm.setDarkMode(true);

  // Add server address parameter
  wm.addParameter(&serverAddress);
  
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
  const String hostname = WiFi.getHostname();
  int httpCode = http.POST("{ \"nodeId\" : \"" + hostname + "\" }");

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

void startMDNS() {
  const String hostname = WiFi.getHostname();
  if (MDNS.begin(hostname)) {
    Serial.println("MDNS responder started on " + hostname);
  }
}

void handleRoot() {
  wm.server->send(200, "text/plain", "hello from esp32!");
}

void bindServerCallback() {
  wm.server->on("/index", handleRoot);

  wm.server->on("/inline", []() {
    wm.server->send(200, "text/plain", "this works as well");
  });
}

void startServer() {
  Serial.println("Starting HTTP server");
  wm.setWebServerCallback(bindServerCallback);
  wm.setConfigPortalBlocking(false);
  wm.startWebPortal();
  Serial.println("HTTP server started");
}

void setup() {
  setupSerial();
  setupHostName();
  setupWiFi();
  registerMaster();
  startMDNS();
  startServer();
}

void loop() {
  wm.process();
  delay(2);//allow the cpu to switch to other tasks
}
