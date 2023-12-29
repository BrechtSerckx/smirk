
// Boilerplate for injecting strings from macros
#define ST(A) #A
#define STR(A) ST(A)
#define PrefsNamespace "smirk"

#include <Arduino.h>
#include <ESPmDNS.h>
#include <HTTPClient.h>
#include <Preferences.h>
#include <WiFiManager.h>

#include <SmirkMate.h>

PrintLogger logger = PrintLogger(&Serial);
LogIRSender irSender = LogIRSender(&logger);
RawIRSignal mockIRSignal = RawIRSignal({}, 1000);
MockIRReceiver irReceiver = MockIRReceiver(&logger, mockIRSignal);

// WiFiManager
WiFiManager wm;
WiFiManagerParameter* serverAddressParam;

Preferences preferences;

String getMateId() {
  return WiFi.getHostname();
}

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

void unpairCaptain(String serverAddress, String accessToken) {
  Serial.printf("Pairing to: %s\n", serverAddress.c_str());

  HTTPClient http;
  http.begin(serverAddress + "/unpair");
  http.addHeader("Content-Type", "application/json");

  StaticJsonDocument<1024> doc;
  doc["mateId"] = getMateId();
  doc["accessToken"] = accessToken;
  String requestPayload;
  serializeJson(doc, requestPayload);
  int httpCode = http.POST(requestPayload);

  if(httpCode != HTTP_CODE_OK) {
    Serial.printf("Unpairing failed: %s\n", http.errorToString(httpCode).c_str());
    return;
  }
  http.end();
}

void pairCaptain(String serverAddress, String accessToken) {
  Serial.printf("Pairing to: %s\n", serverAddress.c_str());

  HTTPClient http;
  http.begin(serverAddress + "/pair");
  http.addHeader("Content-Type", "application/json");

  StaticJsonDocument<1024> requestDoc;
  requestDoc["mateId"] = getMateId();
  requestDoc["baseUrl"] = "http://" + WiFi.localIP().toString();
  if (accessToken != "") requestDoc["accessToken"] = accessToken;
  String requestPayload;
  serializeJson(requestDoc, requestPayload);

  int httpCode = http.POST(requestPayload);
  if(httpCode != HTTP_CODE_OK) {
    Serial.printf("Pairing failed: %s\n", http.errorToString(httpCode).c_str());
    return;
  }

  String responsePayload = http.getString();
  Serial.println(responsePayload); // FIXME: remove
  DynamicJsonDocument responseDoc(1024);
  deserializeJson(responseDoc, responsePayload);
  JsonObject responseObj = responseDoc.as<JsonObject>();
  const char* newAccessToken = responseObj["accessToken"];
  if (newAccessToken) {
    preferences.begin(PrefsNamespace, false);
    preferences.putString("access_token", String(newAccessToken));
    preferences.end();
  } else {
    Serial.println("Unexpected response: " + responsePayload);
  }

  http.end();
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
  String serverAddress = preferences.getString("server_address",String());
  preferences.end();
  serverAddressParam = new WiFiManagerParameter("server_address", "Server Address", serverAddress.c_str(), 50);
  wm.addParameter(serverAddressParam);
  wm.setSaveParamsCallback([]() {
    const String newServerAddress = serverAddressParam->getValue();
    preferences.begin(PrefsNamespace, false);
    const String oldServerAddress = preferences.getString("server_address", String());
    const String accessToken = preferences.getString("access_token", String());
    if (oldServerAddress != newServerAddress)
      preferences.putString("server_address", serverAddressParam->getValue());
    preferences.end();
    if (oldServerAddress != newServerAddress) {
      if (accessToken != "") unpairCaptain(oldServerAddress, accessToken);
      if (newServerAddress != "") pairCaptain(newServerAddress, accessToken);
    }
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
    preferences.begin(PrefsNamespace, false);
    const String accessToken = preferences.getString("access_token", String());
    preferences.end();
    if (accessToken == "" || authHeader != accessToken) {
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
  preferences.begin(PrefsNamespace, true);
  const String serverAddress = preferences.getString("server_address",String());
  const String accessToken = preferences.getString("access_token", String());
  preferences.end();
 if (serverAddress != "") pairCaptain(serverAddress, accessToken);
  startMDNS();
  startServer();
}

void loop() {
  wm.process();
  delay(2);//allow the cpu to switch to other tasks
}
