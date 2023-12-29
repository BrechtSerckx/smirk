#include <Arduino.h>
#include <Smirk.cpp>

Logger logger;
IRSender irSender;
IRReceiver irReceiver;

void setup() {
  logger = PrintLogger(&Serial);
  irSender = LogIRSender(&logger);
  RawIRSignal mockIRSignal = RawIRSignal({}, 1000);
  irReceiver = MockIRReceiver(&logger, mockIRSignal);
}

void loop() {
}
