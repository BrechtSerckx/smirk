#pragma once

class IRSender;

class IRSignal {
  public:
    virtual void send(IRSender &sender) = 0;
};
