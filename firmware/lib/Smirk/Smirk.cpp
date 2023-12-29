#include <Arduino.h>
#include <vector>

class Logger {
public:
  virtual void log(String msg);
};

class PrintLogger : public Logger {
private:
  Print *print;
public:
  PrintLogger(Print *_print)
    : print(_print) {
  }
  void log(const String msg) {
    this->print->println(msg);
  };
};

class IRSender;
class IRSignal;
class RawIRSignal;

class IRSender {
public:
  virtual void sendRaw(const uint16_t buf[],
                       const uint16_t len,
                       const uint16_t hz);
};

class IRReceiver {
public:
  virtual RawIRSignal receiveRaw();
};

class IRSignal {
public:
  virtual void send(IRSender *sender);
};

class RawIRSignal : public IRSignal {
private:
  const std::vector<uint16_t> buf;
  const uint16_t len;
  const uint16_t hz;
public:
  RawIRSignal(const uint16_t _buf[],
              const uint16_t _len,
              const uint16_t _hz)
    : buf(std::vector<uint16_t>(_buf, _buf + _len)),
      len(_len),
      hz(_hz) {
  };
  void send(IRSender *sender) {
    sender->sendRaw(&this->buf[0], this->len, this->hz);
  };
};

class LogIRSender : public IRSender {
private:
  Logger *logger;
public:
  LogIRSender(Logger *_logger) {
    this->logger = _logger;
  };
  void sendRaw(const uint16_t buf[],
               const uint16_t len,
               const uint16_t hz) {
    this->logger->log("Sending raw IR signal.");
    return;
  };
};

class MockIRReceiver : public IRReceiver {
private:
  Logger *logger;
  const RawIRSignal &mockSignal;
public:
  MockIRReceiver(Logger *_logger,
                 RawIRSignal &_mockSignal)
    : mockSignal(_mockSignal) {
    this->logger = _logger;
  };
  RawIRSignal receiveRaw() {
    this->logger->log("Receiving raw IR signal.");
    return this->mockSignal;
  };
};
