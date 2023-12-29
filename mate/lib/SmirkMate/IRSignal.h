#pragma once

#include <ArduinoJson.h>

class IRSender;

class IRSignal {
  public:
    virtual void send(IRSender &sender) = 0;

  static IRSignal* decodeJson(JsonObject obj);
};
