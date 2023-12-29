#include <stdint.h>
#include <vector>

#include <IRSender/Log.h>

LogIRSender::LogIRSender(Logger *_logger) {
  this->logger = _logger;
};

void LogIRSender::sendRaw(const std::vector<uint16_t> _buf,
             const uint16_t hz) {
  this->logger->log("Sending raw IR signal.");
  return;
};
