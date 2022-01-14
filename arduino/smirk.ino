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

#define BAUDRATE 9600

void setup() {
  // put your setup code here, to run once:

  // initialize LED
  pinMode(LED_BUILTIN, OUTPUT);

  // start serial port at 9600 bps:
  Serial.begin(BAUDRATE);
  while (!Serial) {
    ; // wait for serial port to connect. Needed for native USB port only
  }
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
      default:
        Serial.print("Error: unrecognized command: ");
        Serial.println(cmd,HEX);
    }
  }
}
