/* #define CMD_NULL 0x00 */
#define CMD_NULL 0x30
/* #define CMD_PING 0x01 */
#define CMD_PING 0x31
/* #define CMD_VERSION 0x02 */
#define CMD_VERSION 0x32
/* #define CMD_ADD 0x03 */
#define CMD_ADD 0x33
/* #define CMD_SEND 0x04 */
#define CMD_SEND 0x34
/* #define CMD_READ 0x05 */
#define CMD_READ 0x35

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
 * RECEIVER
 */

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

/*
 * 
 */

void setup() {
  // put your setup code here, to run once:

  // initialize LED
  pinMode(LED_BUILTIN, OUTPUT);

  setupReceiver();

  setupSerial();


}

void loop() {
  // put your main code here, to run repeatedly:
  if (Serial.available() > 0) {
    byte cmd = Serial.read();
    switch (cmd) {
      case CMD_NULL:
        {}
        break;
      case CMD_PING:
        {
          Serial.write(CMD_PING);
          Serial.print("\r\n");
        }
        break;
      case CMD_VERSION:
        {
          Serial.println("dev");
        }
        break;
      case CMD_ADD:
        {
          long int n = Serial.parseInt();
          if ( n == 0 ) {
            Serial.println("Error: ADD command did not receive a number > 0");
          } else {
            n++;
            Serial.println(n);
          }
        }
        break;
      case CMD_SEND:
        {
          digitalWrite(LED_BUILTIN, HIGH);
          delay(100);
          digitalWrite(LED_BUILTIN, LOW);
        }
        break;
      case CMD_READ:
        {
          Serial.println(receiveCount);
        }
        break;
      default:
        Serial.print("Error: unrecognized command: ");
        Serial.println(cmd,HEX);
    }
  }
}
