#include <Arduino.h>

#include <Smirk.h>

PrintLogger logger = PrintLogger(&Serial);
LogIRSender irSender = LogIRSender(&logger);
RawIRSignal mockIRSignal = RawIRSignal({}, 1000);
MockIRReceiver irReceiver = MockIRReceiver(&logger, mockIRSignal);

setupSerial() {
  Serial.begin(9600);
}

void setup() {
  setupSerial();
}

void loop() {
}
