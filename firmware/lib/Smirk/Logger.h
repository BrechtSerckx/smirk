#pragma once

#include <Arduino.h>

class Logger {
public:
  virtual void log(const String &msg);
 };
