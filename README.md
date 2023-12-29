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

- Master server written in Haskell. User interaction will happen through this server, and it will contain a database.
- One or more nodes. These are relatively dumb, and only execute commands from the server. They do have a barebones web interface in order to connect to a network and pair with a master.
- The master e.g. runs in a docker container in the network.
- Nodes run in the same network as their master.
- Nodes need to pair to a master. 

## Pairing

For pairing, the node and master exchange data so that the node is connected to the right master, and so that only the master node can send commands to the node.
First the node first needs to know about the master. The IP address of the master is entered by the user, possibly from mDNS search.
Then the node registers itself to the master, possibly involving a pairing code that's shown on both the node and master web UI.
Then the master sends a secret to the node. The master will send this secret with on each command to the node, so the node can verify that the sender really is the master.
