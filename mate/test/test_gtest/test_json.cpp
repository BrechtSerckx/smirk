#include <gtest/gtest.h>

#include <cmath>
#include <Arduino.h>
#include <ArduinoJson.h>

#include "IRSignal/Raw.h"

TEST(Json, Example)
{
  DynamicJsonDocument doc(1024);
  std::string input =
    "{\"sensor\":\"gps\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";
  deserializeJson(doc, input);
  JsonObject obj = doc.as<JsonObject>();
  EXPECT_EQ(obj["sensor"], "gps");
  EXPECT_EQ(obj["time"], 1351824120);
  EXPECT_EQ(std::floor((float) obj["data"][0] * 100.0) / 100.0, 48.75);
  EXPECT_EQ(std::floor((float) obj["data"][1] * 100.0) / 100.0, 2.3);
 }

TEST(Json, DecodeRawSignal)
{
  DynamicJsonDocument doc(1024);
  std::string input =
    "{\"hz\":42,\"buf\":[12,13,14,15]}";
  deserializeJson(doc, input);
  JsonObject obj = doc.as<JsonObject>();
  RawIRSignal* res = RawIRSignal::decodeJson(obj);
  ASSERT_NE(res, nullptr);
  EXPECT_EQ((*res).hz, 42);
  EXPECT_EQ((*res).buf[0], 12);
  EXPECT_EQ((*res).buf[1], 13);
}

TEST(Json, DecodeRawSignal2)
{
  DynamicJsonDocument doc(1024);
  std::string input =
    "{\"type\":\"raw\",\"hz\":42,\"buf\":[12,13,14,15]}";
  deserializeJson(doc, input);
  JsonObject obj = doc.as<JsonObject>();
  RawIRSignal* res = dynamic_cast<RawIRSignal*>(IRSignal::decodeJson(obj));
  ASSERT_NE(res, nullptr);
  EXPECT_EQ((*res).hz, 42);
  EXPECT_EQ((*res).buf[0], 12);
  EXPECT_EQ((*res).buf[1], 13);
}
