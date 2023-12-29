#include <Arduino.h>

#include <Smirk.h>

PrintLogger logger = PrintLogger(&Serial);
LogIRSender irSender = LogIRSender(&logger);
RawIRSignal mockIRSignal = RawIRSignal({}, 1000);
MockIRReceiver irReceiver = MockIRReceiver(&logger, mockIRSignal);

void setupSerial() {
  Serial.begin(SERIAL_BAUD_RATE);
}

void setup() {
  setupSerial();
}

void loop() {
}
