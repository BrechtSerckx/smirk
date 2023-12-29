#include <stdint.h>
#include <vector>

#include <IRSender.h>

#include <IRSignal/Raw.h>

RawIRSignal::RawIRSignal(const std::vector<uint16_t> &_buf,
                         const uint16_t _hz)
  : buf(_buf),
    hz(_hz) {};
void RawIRSignal::send(IRSender *sender) {
  sender->sendRaw(this->buf, this->hz);
};
