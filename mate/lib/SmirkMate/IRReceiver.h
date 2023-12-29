#pragma once

#include <stdint.h>

#include "IRSignal.h"

class IRReceiver {
public:
  virtual RawIRSignal receiveRaw() = 0;
};
