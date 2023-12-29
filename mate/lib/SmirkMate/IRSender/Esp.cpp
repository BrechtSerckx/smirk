#ifdef ARDUINO

#include <stdint.h>
#include <vector>

#include "IRsend.h"

#include "IRSender/Esp.h"

EspIRSender::EspIRSender(IRsend *_irSend) {
  this->irSend = _irSend;
};
void EspIRSender::sendRaw(const std::vector<uint16_t> &buf,
                          const uint16_t hz) {
  this->irSend->sendRaw(&buf[0], buf.size(), hz);
};

#endif
