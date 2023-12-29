
// Boilerplate for injecting strings from macros
#define ST(A) #A
#define STR(A) ST(A)
#define PrefsNamespace "smirk"

#include <Arduino.h>
#include <ESPmDNS.h>
#include <HTTPClient.h>
#include <Preferences.h>
#include <WiFiManager.h>

#include <Smirk.h>

PrintLogger logger = PrintLogger(&Serial);
LogIRSender irSender = LogIRSender(&logger);
RawIRSignal mockIRSignal = RawIRSignal({}, 1000);
MockIRReceiver irReceiver = MockIRReceiver(&logger, mockIRSignal);

// WiFiManager
WiFiManager wm;
WiFiManagerParameter* serverAddressParam;

Preferences preferences;

// Register
String accessToken = "";
bool registered = false;

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

  //WM config
  wm.setTitle("SMIRK");
  wm.setDarkMode(true);
  wm.setParamsPage(true);
  wm.setShowInfoErase(true);
  wm.setShowInfoUpdate(false);

  // Add server address parameter
  preferences.begin(PrefsNamespace, true);
  serverAddressParam = new WiFiManagerParameter("server_address", "Server Address", preferences.getString("server_address","").c_str(), 50);
  preferences.end();
  wm.addParameter(serverAddressParam);
  wm.setSaveParamsCallback([]() {
    preferences.begin(PrefsNamespace, false);
    preferences.putString("server_address", serverAddressParam->getValue());
    preferences.end();
  });
  
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
  preferences.begin(PrefsNamespace, true);
  String serverAddress = preferences.getString("server_address","");
  preferences.end();
  http.begin(serverAddress + "/register");

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
      Serial.println(payload); // FIXME: remove
      DynamicJsonDocument doc(1024);
      deserializeJson(doc, payload);
      JsonObject obj = doc.as<JsonObject>();
      const char* newAccessToken = obj["accessToken"];
      if (newAccessToken) {
        accessToken = String(newAccessToken);
        registered = true;
      } else {
        Serial.println("Unexpected response: " + payload);
      }
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
  wm.server->on("/index", HTTP_GET, handleRoot);

  wm.server->on("/send", HTTP_POST, []() {
    String authHeader = wm.server->header("Authorization");
    if (registered == false || authHeader != accessToken) {
      wm.server->send(403, "text/plain", "Invalid access token.");
    }
    if (wm.server->hasArg("plain") == false) {
      wm.server->send(400, "text/plain", "No payload.");
    } else {
      String requestBody = wm.server->arg("plain");
      Serial.println("Received POST request body:");
      Serial.println(requestBody);
      DynamicJsonDocument doc(1024);
      deserializeJson(doc, requestBody);
      JsonObject obj = doc.as<JsonObject>();
      RawIRSignal* signal = RawIRSignal::decodeJson(obj);
      if (signal == nullptr) {
        wm.server->send(400, "text/plain", "Unable to decode IR signal.");
      } else {
        signal->send(irSender);
        wm.server->send(200, "text/plain", "Done.");
      }
    }
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
