#pragma once

#include <stdint.h>
#include <vector>

#include "Logger.h"
#include "IRSender.h"

class LogIRSender : public IRSender {
private:
  Logger *logger;
public:
  LogIRSender(Logger *_logger);
  void sendRaw(const std::vector<uint16_t> &_buf,
               const uint16_t hz);
};
