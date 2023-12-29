#include <gtest/gtest.h>

#include <cmath>
#include <list>
#include <tuple>
#include <vector>

#include <Arduino.h>
#include <ArduinoJson.h>

#include "IRSender.h"
#include "IRSignal.h"
#include "IRSignal/Raw.h"
#include "IRSendController.h"

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

class MockIRSender : public IRSender {
public:
  std::list<std::tuple<std::vector<uint16_t>, uint16_t>> signalsSent = {};
  void sendRaw(const std::vector<uint16_t> &buf,
               const uint16_t hz) {
    this->signalsSent.push_back(std::make_tuple(buf, hz));
  };
};
TEST(IRSender, controlInversion)
{
  std::vector<uint16_t> buf = {1, 2};
  uint16_t hz = 1000;
  RawIRSignal mockIRSignal = RawIRSignal(buf, hz);
  MockIRSender irSender = MockIRSender();
  mockIRSignal.send(irSender);
  
  std::list<std::tuple<std::vector<uint16_t>, uint16_t>> expectedSignalsSent
    = { std::make_tuple(buf, hz)};
  EXPECT_EQ(irSender.signalsSent, expectedSignalsSent);
}
TEST(IRSendController, sendSignal)
{
  std::vector<uint16_t> buf = {1, 2};
  uint16_t hz = 1000;
  RawIRSignal mockIRSignal = RawIRSignal(buf, hz);
  MockIRSender irSender = MockIRSender();
  IRSendController ctrl = IRSendController(&irSender);

  ctrl.sendSignal(mockIRSignal);
  
  std::list<std::tuple<std::vector<uint16_t>, uint16_t>> expectedSignalsSent
    = { std::make_tuple(buf, hz)};
  EXPECT_EQ(irSender.signalsSent, expectedSignalsSent);
}
