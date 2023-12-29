/*
 Copyright (c) 2014-present PlatformIO <contact@platformio.org>

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
**/

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
