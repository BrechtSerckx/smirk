#pragma once

#include <mutex>

#include "IRSender.h"
#include "IRSignal.h"

class IRSendController {
 private:
  IRSender *irSender;
  std::mutex irSenderMutex;
  // Disable copy, as instances contain a mutex.
  IRSendController(const IRSendController&) = delete;

 protected:
  void sendSignal(IRSignal &irSignal);

 public:
  IRSendController(IRSender *_irSender);
  void sendSignalLocking(IRSignal &irSignal);
};
 
