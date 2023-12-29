
// Boilerplate for injecting strings from macros
#define ST(A) #A
#define STR(A) ST(A)

#include <Arduino.h>
#include <ESPmDNS.h>
#include <HTTPClient.h>
#include <WiFiManager.h>
#include "WebServer.h"

#include <Smirk.h>

PrintLogger logger = PrintLogger(&Serial);
LogIRSender irSender = LogIRSender(&logger);
RawIRSignal mockIRSignal = RawIRSignal({}, 1000);
MockIRReceiver irReceiver = MockIRReceiver(&logger, mockIRSignal);
WebServer server(80);

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
  server.send(200, "text/plain", "hello from esp32!");
}

void handleNotFound() {
  String message = "File Not Found\n\n";
  message += "URI: ";
  message += server.uri();
  message += "\nMethod: ";
  message += (server.method() == HTTP_GET) ? "GET" : "POST";
  message += "\nArguments: ";
  message += server.args();
  message += "\n";
  for (uint8_t i = 0; i < server.args(); i++) {
    message += " " + server.argName(i) + ": " + server.arg(i) + "\n";
  }
  server.send(404, "text/plain", message);
}

void startServer() {
  Serial.println("Starting HTTP server");
  server.on("/", handleRoot);

  server.on("/inline", []() {
    server.send(200, "text/plain", "this works as well");
  });

  server.onNotFound(handleNotFound);

  server.begin();
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
  server.handleClient();
  delay(2);//allow the cpu to switch to other tasks
}
