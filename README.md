SMIRK
===

Smart InfraRed Kit

## Domain study

Links:

- https://hardwarefun.com/tutorials/compiling-arduino-sketches-using-makefile
- https://learn.adafruit.com/using-an-infrared-library/sending-ir-codes


- [IR Sensor](https://learn.adafruit.com/ir-sensor)
  Dated: 2012/2022
  Parts:
  - [ ] [TSOP38238](https://nl.rs-online.com/web/p/products/7085086/)
- [Using an Infrared Library on Arduino](https://learn.adafruit.com/using-an-infrared-library)
  Dated: 2015/2022
  Parts:
  - [ ] Arduino
  - [x] TSOP38238
  - [ ] [R470](https://nl.rs-online.com/web/p/products/1251133/)
  - [ ] [2N2222](https://nl.rs-online.com/web/p/products/7390381/)
- [Building the Assistive Technology Ultimate Remote](https://learn.adafruit.com/building-the-assistive-technology-ultimate-remote/)
  Dated: 2019/2022
- [Building an Infrared Transmitter and Receiver Board](https://learn.adafruit.com/building-an-infrared-transmitter-and-receiver-board/overview)
  Dated: 2019/2022
  Parts:
  - [ ] [2 infrared LEDs](https://www.adafruit.com/product/387)
  - [x] TSOP38238
  - [x] [PN2222](https://www.adafruit.com/product/3599)
  - [ ] [PN2907](https://www.adafruit.com/product/3599)
  - [ ] [R1k]
  - [ ] [R33]
  - [ ] [TSMP58000](https://nl.rs-online.com/web/p/ir-receivers/7730297)

- [IOT IR Remote using Raspberry Pi Zero W and QtPy Hat](https://learn.adafruit.com/iot-ir-remote-using-raspberry-pi-zero-w-and-qtpy-hat)
  Dated: 2021/2021
  Parts:
  - [ ] Raspberry Pi Zero WH
  - [ ] [Adafruit QT Py - SAMD21](https://www.adafruit.com/product/4600)
  - [ ] Power supply 5V 2.5A
  - [x] [2 infrared LEDs](https://www.adafruit.com/product/387)
  - [x] [PN2222](https://www.adafruit.com/product/3599)
  - [x] [PN2907](https://www.adafruit.com/product/3599)
  - [x] [R1k]
  - [x] [R33]
  - [x] TSOP38238
  - [ ] [2x2- GPIO header](https://www.adafruit.com/product/2222)

## SMIRK design

- Raspberry Pi 3b / Zero
- Arduino Leonardo
- Breakout board (infrared transmitter and receiver board)
- Main parts:
  - [ ] [TSOP38238](https://nl.rs-online.com/web/p/products/7085086/)
  - [ ] [TSMP58000](https://nl.rs-online.com/web/p/ir-receivers/7730297)
  - [ ] [PN2222](https://www.adafruit.com/product/3599)
  - [ ] [PN2907](https://www.adafruit.com/product/3599)
  - [ ] [Infrared LEDs](https://www.adafruit.com/product/387)
  - [ ] [R1k >25mW]
  - [ ] [R33 >1W]
- Misc parts:
  - [ ] [R470 >60mW](https://nl.rs-online.com/web/p/products/1251133/)
- Future parts:
  - [ ] [IRLB8721PbF](https://www.adafruit.com/product/355)

### How to use the Arduino Leonardo

```
./build.sh DIRECTORY COMMAND

DIRECTORY project directory containing arduino source files
COMMAND   `Arduino-Makefile` command
```

- `build`: Builds the project
- `upload`: Uploads the project to the Arduino
- `monitor`: Opens `picocom` as terminal for the serial monitor
   Press `C-a C-x` to exit the `picocom` command.
   After resetting, wait for the `RX` LED to flash before opening the monitor.

