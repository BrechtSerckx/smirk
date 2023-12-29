#pragma once

#include <stdint.h>
#include <vector>

class IRSender {
public:
  virtual void sendRaw(const std::vector<uint16_t> &_buf,
                       const uint16_t hz) = 0;
};
