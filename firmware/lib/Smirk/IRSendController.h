#pragma once

#include "IRSender.h"
#include "IRSignal.h"

class IRSendController {
 private:
  IRSender *irSender;

 public:
  IRSendController(IRSender *_irSender);
  void sendSignal(IRSignal &irSignal);
};
