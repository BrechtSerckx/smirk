#include <gtest/gtest.h>

#include <cmath>
#include <Arduino.h>
#include <ArduinoJson.h>

#include "IRSignal.h"
#include "IRSignal/Raw.h"

TEST(RawIRSignal, decodeJson)
{
  DynamicJsonDocument doc(1024);
  std::string input =
    "{\"hz\":38000,\"buf\":[9000, 4500]}";
  deserializeJson(doc, input);
  JsonObject obj = doc.as<JsonObject>();
  RawIRSignal s = RawIRSignal::decodeJson(obj);
  uint16_t hz = s.hz;
  EXPECT_EQ(hz, 38000);
  EXPECT_EQ(s.buf, std::vector<uint16_t>({9000, 4500}));
 }
