
#include <mutex>

#include "IRSendController.h"

#include "IRSender.h"
#include "IRSignal.h"

IRSendController::IRSendController(IRSender *_irSender) {
  this->irSender = _irSender;
};
void IRSendController::sendSignal(IRSignal &irSignal) {
  irSignal.send(*this->irSender);
}

void IRSendController::sendSignalLocking(IRSignal &irSignal) {
  const std::lock_guard<std::mutex> lock(irSenderMutex);
  irSignal.send(*this->irSender);
}
