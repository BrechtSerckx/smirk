#include <Arduino.h>

#include "Logger/Print.h"

PrintLogger::PrintLogger(Print *_print)
  : print(_print) {
  if (!print) {
    throw std::invalid_argument("Print pointer cannot be nullptr");
  }
}
void PrintLogger::log(const String &msg) {
  this->print->println(msg);
};
