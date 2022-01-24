#include <ArduinoJson.h>
#include <IRLibDecodeBase.h>
#include <IRLibSendBase.h>
/* #include <IRLib_P01_NEC.h> */
/* #include <IRLib_P02_Sony.h> */
#include <IRLib_P03_RC5.h>
#include <IRLib_P04_RC6.h>
/* #include <IRLib_P05_Panasonic_Old.h> */
/* #include <IRLib_P06_JVC.h> */
/* #include <IRLib_P07_NECx.h> */
/* #include <IRLib_P08_Samsung36.h> */
/* #include <IRLib_P09_GICable.h> */
/* #include <IRLib_P10_DirecTV.h> */
/* #include <IRLib_P11_RCMM.h> */
/* #include <IRLib_P12_CYKM.h> */
#include <IRLibCombo.h>
#include <IRLibRecvPCI.h> 

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

StaticJsonDocument<400> noOp() {
  StaticJsonDocument<400> doc;
  doc["success"] = true;
  doc["data"] = "";
  return doc;
}

/*
 * PING
 */

StaticJsonDocument<400> pong() {
  StaticJsonDocument<400> doc;
  doc["success"] = true;
  doc["data"] = "pong";
  return doc;
}

/*
 * VERSION
 */

#define VERSION "dev"
StaticJsonDocument<400> version() {
  StaticJsonDocument<400> doc;
  doc["success"] = true;
  doc["data"] = VERSION;
  return doc;
}

/*
 * ADD
 */

StaticJsonDocument<400> add(int n) {
  StaticJsonDocument<400> doc;
  if ( n <= 0) {
    doc["success"] = "Failure";
    doc["data"] = "ADD command did not receive a number > 0";
  } else {
    n++;
    doc["success"] = true;
    doc["data"] = n;
  }
  return doc;
}

/*
 * SENDER
 */

IRsend mySender;
void setupSender() {
  pinMode(LED_BUILTIN, OUTPUT);
}

StaticJsonDocument<400> send(uint8_t protocolNum, uint32_t value, uint8_t bits, uint16_t address, uint8_t freq=38) {
  digitalWrite(LED_BUILTIN, HIGH);
  mySender.send(protocolNum, value, bits, freq);
  digitalWrite(LED_BUILTIN, LOW);
  StaticJsonDocument<400> doc;
  doc["success"] = true;
  doc["data"] = "";
  return doc;
}

/*
 * RECEIVER
 */

#define RECEIVER_PIN 2
IRrecvPCI myReceiver(RECEIVER_PIN);
IRdecode myDecoder;
uint8_t protocolNum = UNKNOWN;
uint32_t value = 0x0;
uint16_t bits = 0;
uint8_t address = 0x0;

void setupReceiver() {
  delay(2000); 
  myReceiver.enableIRIn();
}
void checkReceiver() {
  if (myReceiver.getResults()) {
    myDecoder.decode();
    protocolNum = myDecoder.protocolNum;
    value = myDecoder.value;
    bits = myDecoder.bits;
    address = myDecoder.address;
    myReceiver.enableIRIn();
  }
}

StaticJsonDocument<400> receive() {
  StaticJsonDocument<400> doc;
  doc["success"] = true;
  doc["data"]["protocolNum"] = protocolNum;
  doc["data"]["value"] = value;
  doc["data"]["bits"] = bits;
  doc["data"]["address"] = address;
  return doc;
}

/*
 *  MAIN
 */

void setup() {
  // put your setup code here, to run once:
  setupSender();
  setupSerial();
  setupReceiver();
}

void loop() {
  // put your main code here, to run repeatedly:

  // check IR receiver
  checkReceiver();

  // check for commands
  if (Serial.available() > 0) {
    StaticJsonDocument<400> doc;
    StaticJsonDocument<400> resp;
    DeserializationError err = deserializeJson(doc, Serial);
    while (Serial.available() > 0) {
      Serial.read();
    }
    if (err == DeserializationError::Ok) {
      const char* cmd = doc["cmd"];
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
        const uint8_t protocolNum = doc["data"]["protocolNum"];
        const uint32_t value = doc["data"]["value"];
        const uint16_t bits = doc["data"]["bits"];
        const uint8_t address = doc["data"]["address"];
        resp = send(protocolNum, value, bits, address);
      } else if (strcmp(cmd, "Receive") == 0) {
        resp = receive();
      } else {
        resp["success"] = false;
        resp["data"] = strcat("Unrecognized command: " , cmd);
      }
    } else {
      resp["success"] = false;
      resp["data"] = strcat("Invalid json: ", err.c_str());
    }
    serializeJson(resp, Serial);
    Serial.write('\n');
    Serial.flush();
  }
}
