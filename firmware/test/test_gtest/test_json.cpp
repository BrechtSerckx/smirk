#include <gtest/gtest.h>

#include <cmath>
#include <Arduino.h>
#include <ArduinoJson.h>

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
