
#include "IRSendController.h"

#include "IRSender.h"
#include "IRSignal.h"

IRSendController::IRSendController(IRSender *_irSender) {
  this->irSender = _irSender;
};
void IRSendController::sendSignal(IRSignal &irSignal) {
  irSignal.send(*this->irSender);
}
