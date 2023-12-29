#pragma once

#include <stdint.h>

#include "Logger.h"
#include "IRReceiver.h"
#include "IRSignal/Raw.h"

class MockIRReceiver : public IRReceiver {
private:
  Logger *logger;
  const RawIRSignal &mockSignal;
public:
  MockIRReceiver(Logger *_logger,
                 RawIRSignal &_mockSignal);
  RawIRSignal receiveRaw();
};
