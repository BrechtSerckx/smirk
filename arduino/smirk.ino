/*
 * SERIAL
 */

#define BAUDRATE 9600

void setupSerial() {
  // start serial port at 9600 bps:
  Serial.begin(BAUDRATE);
  while (!Serial) {
    ; // wait for serial port to connect. Needed for native USB port only
  }
}

/*
 * OK
 */

#define RES_OK byte(0x00)

/*
 * NOOP
 */

#define CMD_NULL 0x00
void noop() {
  Serial.write(RES_OK);
}

/*
 * PING
 */

#define CMD_PING 0x01
void pong() {
  Serial.write(CMD_PING);
}

/*
 * VERSION
 */

#define CMD_VERSION 0x02
#define VERSION "dev"
void version() {
  Serial.write(VERSION);
}

/*
 * ADD
 */

#define CMD_ADD 0x03
void add() {
  long int n = Serial.parseInt();
  if ( n == 0 ) {
    Serial.print("Error: ADD command did not receive a number > 0");
  } else {
    n++;
    Serial.print(n);
  }
}

/*
 * SENDER
 */

#define CMD_SEND 0x04

void setupSender() {
  pinMode(LED_BUILTIN, OUTPUT);
}

void send() {
  digitalWrite(LED_BUILTIN, HIGH);
  delay(100);
  digitalWrite(LED_BUILTIN, LOW);
  Serial.write(RES_OK);
}

/*
 * RECEIVER
 */

#define CMD_RECEIVE 0x05

#define RECEIVER_PIN 2
unsigned short receiveCount = 0;
unsigned long lastReceiverInterrupt = 0;

void receiverInterrupt() {
  unsigned long currentTime = millis();
  if (currentTime - lastReceiverInterrupt > 400) {
    receiveCount++;
    lastReceiverInterrupt = currentTime;
  }
}

void setupReceiver() {
  pinMode(RECEIVER_PIN, INPUT);
  attachInterrupt(digitalPinToInterrupt(RECEIVER_PIN), receiverInterrupt, RISING);
}

void receive() {
  Serial.print(receiveCount);
}

/*
 *  MAIN
 */

void setup() {
  // put your setup code here, to run once:
  setupSender();
  setupReceiver();
  setupSerial();
}

void loop() {
  // put your main code here, to run repeatedly:
  if (Serial.available() > 0) {
    byte cmd = Serial.read();
    switch (cmd) {
      case CMD_NULL:
        noop();
        break;
      case CMD_PING:
        pong();
        break;
      case CMD_VERSION:
        version();
        break;
      case CMD_ADD:
        add();
        break;
      case CMD_SEND:
        send();
        break;
      case CMD_RECEIVE:
        receive();
        break;
      default:
        Serial.print("Error: unrecognized command: ");
        Serial.print(cmd, HEX);
    }
  }
}
