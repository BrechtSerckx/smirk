SMIRK
===

Smart InfraRed Kit

## SMIRK design

- ESP MCU
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


## Architecture

- Captain written in Haskell. User interaction will happen through this captain, and it will contain a database.
- One or more mates. These are relatively dumb, and only execute commands from the captain. They do have a barebones web interface in order to connect to a network and pair with the captain.
- The captain e.g. runs in a docker container in the network.
- Mates run in the same network as their captain.
- Mates need to pair to a captain. 

## Pairing

For pairing, the mate and captain exchange data so that the mate is connected to the right captain, and so that only the captain mate can send commands to the mate.
First the mate first needs to know about the captain. The IP address of the captain is entered by the user, possibly from mDNS search.
Then the mate registers itself to the captain, possibly involving a pairing code that's shown on both the mate and captain web UI.
Then the captain sends a secret to the mate. The captain will send this secret with on each command to the mate, so the mate can verify that the sender really is the captain.
