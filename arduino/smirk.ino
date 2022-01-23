#include <ArduinoJson.h>
#include <StreamUtils.h>
/*
 * SERIAL
 */

#define BAUDRATE 9600

void setupSerial() {
  // start serial port at 9600 bps:
  Serial.begin(BAUDRATE);
  while (!Serial) {
    ; // wait for serial port to connect. Needed for native USB port only
  }
}

/*
 * NOOP
 */

StaticJsonDocument<300> noOp() {
  StaticJsonDocument<300> doc;
  doc["type"] = "Success";
  doc["data"] = "";
  return doc;
}

/*
 * PING
 */

StaticJsonDocument<300> pong() {
  StaticJsonDocument<300> doc;
  doc["type"] = "Success";
  doc["data"] = "pong";
  return doc;
}

/*
 * VERSION
 */

#define VERSION "dev"
StaticJsonDocument<300> version() {
  StaticJsonDocument<300> doc;
  doc["type"] = "Success";
  doc["data"] = VERSION;
  return doc;
}

/*
 * ADD
 */

StaticJsonDocument<300> add(int n) {
  StaticJsonDocument<300> doc;
  if ( n <= 0) {
    doc["type"] = "Failure";
    doc["data"] = "ADD command did not receive a number > 0";
  } else {
    n++;
    doc["type"] = "Success";
    doc["data"] = n;
  }
  return doc;
}

/*
 * SENDER
 */

void setupSender() {
  pinMode(LED_BUILTIN, OUTPUT);
}

StaticJsonDocument<300> send(uint8_t protocolNum, uint32_t value, uint8_t bits, uint16_t address) {
  digitalWrite(LED_BUILTIN, HIGH);
  delay(300);
  digitalWrite(LED_BUILTIN, LOW);
  StaticJsonDocument<300> doc;
  doc["type"] = "Success";
  doc["data"] = "";
  return doc;
}

/*
 * RECEIVER
 */

#define RECEIVER_PIN 2
unsigned short receiveCount = 0;
unsigned long lastReceiverInterrupt = 0;

void receiverInterrupt() {
  unsigned long currentTime = millis();
  if (currentTime - lastReceiverInterrupt > 400) {
    receiveCount++;
    lastReceiverInterrupt = currentTime;
  }
}

void setupReceiver() {
  pinMode(RECEIVER_PIN, INPUT);
  attachInterrupt(digitalPinToInterrupt(RECEIVER_PIN), receiverInterrupt, RISING);
}

StaticJsonDocument<300> receive() {
  StaticJsonDocument<300> doc;
  doc["type"] = "Success";
  doc["data"] = receiveCount;
  return doc;
}

/*
 *  MAIN
 */

void setup() {
  // put your setup code here, to run once:
  setupSender();
  setupReceiver();
  setupSerial();
}

void loop() {
  // put your main code here, to run repeatedly:
  if (Serial.available() > 0) {
    StaticJsonDocument<300> doc;
    StaticJsonDocument<300> resp;
    DeserializationError err = deserializeJson(doc, Serial);
    while (Serial.available() > 0) {
      Serial.read();
    }
    if (err == DeserializationError::Ok) {
      const char* cmd = doc["type"];
      if (strcmp(cmd, "NoOp") == 0) {
        resp = noOp();
      } else if (strcmp(cmd, "Ping") == 0) {
        resp = pong();
      } else if (strcmp(cmd, "Version") == 0) {
        resp = version();
      } else if (strcmp(cmd, "Add") == 0) {
        const int n = doc["data"];
        resp = add(n);
      } else if (strcmp(cmd, "Send") == 0) {
        const uint8_t protocolNum = doc["data"]["protocol"];
        const uint32_t value = doc["data"]["value"];
        const uint16_t bits = doc["data"]["bits"];
        const uint8_t address = doc["data"]["address"];
        resp = send(protocolNum, value, bits, address);
      } else if (strcmp(cmd, "Receive") == 0) {
        resp = receive();
      } else {
        resp["type"] = "Failure";
        resp["data"] = strcat("Unrecognized command: " , cmd);
      }
    } else {
      resp["type"] = "Failure";
      resp["data"] = strcat("Invalid json: ", err.c_str());
    }
    WriteBufferingStream bufferedStream(Serial,64);
    serializeJson(resp, bufferedStream);
    bufferedStream.flush();
  }
}
