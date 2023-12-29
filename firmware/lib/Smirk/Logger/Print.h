#pragma once

#include <Arduino.h>

#include <Logger.h>

class PrintLogger : public Logger {
private:
  Print *print;
public:
  PrintLogger(Print *_print);
  void log(const String &msg) ;
};
