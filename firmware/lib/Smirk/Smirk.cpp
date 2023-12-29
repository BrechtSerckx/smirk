#include <Arduino.h>

class Logger {
public:
  virtual void log(String msg);
};

class PrintLogger : public Logger {
private:
  Print *print;
public:
  PrintLogger(Print *_print) : print(_print) {
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
  virtual void sendRaw(const RawIRSignal &signal);
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
public:
  void send(IRSender *sender) {
    sender->sendRaw(*this);
  };
};

class LogIRSender : public IRSender {
private:
  Logger *logger;
public:
  LogIRSender(Logger *_logger) {
    this->logger = _logger;
  };
  void sendRaw(const RawIRSignal &signal) {
    this->logger->log("Sending raw IR signal.");
    return;
  };
};

class MockIRReceiver : public IRReceiver {
private:
  Logger *logger;
  const RawIRSignal &mockSignal;
public:
  MockIRReceiver(Logger *_logger, RawIRSignal &_mockSignal) : mockSignal(_mockSignal){
    this->logger = _logger;
  };
  RawIRSignal receiveRaw() {
    this->logger->log("Receiving raw IR signal.");
    return this->mockSignal;
  };
};
