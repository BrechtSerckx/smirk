
#include <ArduinoJson.h>

#include "IRSignal.h"
#include "IRSignal/Raw.h"

IRSignal* IRSignal::decodeJson(JsonObject obj) {
  const std::string type = obj["type"];
  if (type == "raw") {
    return RawIRSignal::decodeJson(obj);
  } else {
    return nullptr;
  }
}
