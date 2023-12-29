
#include <stdint.h>

#include <Logger.h>
#include <IRSignal/Raw.h>
#include <IRReceiver.h>

#include <IRReceiver/Mock.h>

MockIRReceiver::MockIRReceiver(Logger *_logger,
                               RawIRSignal &_mockSignal)
  : mockSignal(_mockSignal) {
  this->logger = _logger;
};
RawIRSignal MockIRReceiver::receiveRaw() {
  this->logger->log("Receiving raw IR signal.");
  return this->mockSignal;
};
