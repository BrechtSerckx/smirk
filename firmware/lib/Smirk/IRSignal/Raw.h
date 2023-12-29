#pragma once

#include <stdint.h>
#include <vector>
#include <ArduinoJson.h>

#include "IRSignal.h"

class RawIRSignal : public IRSignal {
 public:
  const std::vector<uint16_t> buf;
  const uint16_t hz;

  RawIRSignal(const std::vector<uint16_t> &_buf,
              const uint16_t _hz);

  void send(IRSender &sender);

  static RawIRSignal decodeJson(JsonObject obj);
};
