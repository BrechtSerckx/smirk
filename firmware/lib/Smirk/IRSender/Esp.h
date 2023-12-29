#pragma once

#include <stdint.h>
#include <vector>
 
#include <IRsend.h>

#include <IRSender.h>

class EspIRSender : public IRSender {
private:
  IRsend *irSend;
public:
  EspIRSender(IRsend *_irSend);
  void sendRaw(const std::vector<uint16_t> &buf,
               const uint16_t hz);
};
