#include <Arduino.h>

#include <Logger/Print.h>

PrintLogger::PrintLogger(Print *_print)
  : print(_print) {
}
void PrintLogger::log(const String msg) {
  this->print->println(msg);
};
