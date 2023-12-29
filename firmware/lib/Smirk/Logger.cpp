#include <Arduino.h>

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
