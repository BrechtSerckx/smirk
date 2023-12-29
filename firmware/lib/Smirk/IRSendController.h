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

 public:
 IRSendController(IRSender *_irSender);
  void sendSignal(IRSignal &irSignal);
};
 
