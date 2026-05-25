This service exposes an API to contorl a broadlink IR remote device reachable over the network.
It makes it possible to programmatically press an IR remote button,
but also listen for and learn new buttons, and save button-press contents under memorable names.

To install:

`$ pip install irremote`

Connect to broadlink device at 10.0.0.12, then repeatedly prompt to listen and save new buttons:

`$ irremote --ip 10.0.0.12 -L`

These are persisted by default under ~/.config/ir-buttons/

List learned buttons:

`$ irremote -l`

Run the HTTP API service on the default port 2727 after connecting to the same broadlink device:

`$ irremote --ip 10.0.0.12`
`$ irremote --ip 10.0.0.12 -p2727`

Run as a client, pressing the button named `LG_VOLUP`:

`$ irremote -b LG_VOLUP`

or

`curl http://localhost:2727/LG_VOLUP_UP`


Press the LG_POWER button, followed by 10 presses of the LG_VOLUP button.

`$ irremote -b LG_POWER,LG_VOLUPx10`

Create a button called "TCL_BRIGHTNESS_UP" that consists of a comma-separated list of other button names.

echo "TCL_MENU,TCL_DOWN,TCL_RIGHT,TCL_RIGHT,TCL_UP,TCL_ENTER,TCL_DOWN,TCL_RIGHT,TCL_UP,TCL_ENTER,TCL_BACKx2" > ~/.config/ir-buttons/TCL_BRIGHTNESS_UP

Press the more complex TCL_BRIGHTNESS_UP button as it if were a regular button:

`$ irremote -b TCL_BRIGHTNESS_UP`
