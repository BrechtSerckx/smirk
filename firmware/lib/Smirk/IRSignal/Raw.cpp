#include <stdint.h>
#include <vector>

#include "IRSender.h"

#include "IRSignal/Raw.h"

RawIRSignal::RawIRSignal(const std::vector<uint16_t> &_buf,
                         const uint16_t _hz)
  : buf(_buf),
    hz(_hz) {};

void RawIRSignal::send(IRSender &sender) {
  sender.sendRaw(this->buf, this->hz);
};

RawIRSignal RawIRSignal::decodeJson(JsonObject obj) {
  std::vector<uint16_t> buf;
  for (JsonVariant e : obj["buf"].as<JsonArray>()) {
    buf.push_back(e.as<int>());
  }
  uint16_t hz = obj["hz"];
  RawIRSignal s(buf, hz);
  return s;
}
