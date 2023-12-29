#include <iomanip>
#include <sstream>
#include <stdint.h>
#include <vector>

#include "IRSender/Log.h"

LogIRSender::LogIRSender(Logger *_logger) {
  this->logger = _logger;
};

void LogIRSender::sendRaw(const std::vector<uint16_t> &buf,
                          const uint16_t hz) {
  std::stringstream ss;
  String result = "RawIRSignal - Frequency: " + String(hz) + " Hz, Buffer: [";

  for (size_t i = 0; i < buf.size(); ++i) {
    result += "0x" + String(buf[i], HEX);
    if (i != buf.size() - 1) {
      result += ", ";
    }
  }

  result += "]";
  this->logger->log("Sending IR signal: "+ result);
  return;
};
