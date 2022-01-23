#include <ArduinoJson.h>
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
 * OK
 */

#define RES_OK byte(0x00)

/*
 * NOOP
 */

void noop() {
  Serial.write(RES_OK);
}

/*
 * PING
 */

#define RES_PONG 0x01
void pong() {
  Serial.write(RES_PONG);
}

/*
 * VERSION
 */

#define VERSION "dev"
void version() {
  Serial.write(VERSION);
}

/*
 * ADD
 */

void add(int n) {
  if ( n == 0 ) {
    Serial.print("Error: ADD command did not receive a number > 0");
  } else {
    n++;
    Serial.print(n);
  }
}

/*
 * SENDER
 */

void setupSender() {
  pinMode(LED_BUILTIN, OUTPUT);
}

void send() {
  digitalWrite(LED_BUILTIN, HIGH);
  delay(100);
  digitalWrite(LED_BUILTIN, LOW);
  Serial.write(RES_OK);
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

void receive() {
  Serial.print(receiveCount);
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
    DeserializationError err = deserializeJson(doc, Serial);
    if (err == DeserializationError::Ok) {
      const char* cmd = doc["type"];
      if (strcmp(cmd, "NoOp") == 0) {
        noop();
      } else if (strcmp(cmd, "Ping") == 0) {
        pong();
      } else if (strcmp(cmd, "Version") == 0) {
        version();
      } else if (strcmp(cmd, "Add") == 0) {
        const int n = doc["data"];
        add(n);
      } else if (strcmp(cmd, "Send") == 0) {
        send();
      } else if (strcmp(cmd, "Receive") == 0) {
        receive();
      } else {
        Serial.print("Error: unrecognized command: ");
        Serial.print(cmd);
      }
    } else {
        Serial.print("Error: invalid json: ");
        Serial.print(err.c_str());
    }
    while (Serial.available() > 0) {
      Serial.read();
    }
  }
}
