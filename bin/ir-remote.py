#!/usr/bin/env python3

import argparse
import broadlink
import http.server
import logging
import os
import pdb
import re
import sys
import time
import traceback

BUTTONS = {
    "SAMSUNG_TV_POWER":
    b'&\x00\x8c\x00\x91\x94\x107\x118\x0f8\x11\x12\x11\x12\x11\x12\x10\x13\x11\x12\x117\x108\x0f7\x11\x12\x11\x13\x10\x13\x11\x12\x10\x13\x10\x13\x117\x10\x13\x11\x12\x10\x13\x13\x10\x10\x13\x11\x12\x117\x10\x13\x115\x127\x117\x0f8\x117\x107\x10\x00\x06C\x92\x93\x116\x117\x107\x11\x13\x10\x14\x11\x11\x10\x13\x10\x13\x118\x0f9\x115\x10\x14\x10\x13\x10\x13\x10\x13\x11\x12\x10\x13\x116\x11\x13\x10\x13\x10\x13\x11\x12\x10\x13\x10\x14\x107\x10\x13\x107\x116\x117\x117\x108\x0f8\x0f\x00\r\x05',
    "SAMSUNG_TV_EXIT":
    b'&\x00b\x00\x04\x15=\x1a\x0e\n\t\x9b\x06<\tC\x06=\x0c\x18\x0cB\x04@\x05\x82\x0f>\x08?\x08\x1e\x04\x16\r\x16\x0eA\x05\x1d\x08<\x0cd\x05?\t\x15\x0eB\x04>\x07\xf5\x04>\x08\x00\x06\\\x04\x12\x08\x07\x13\x062\x07\x05\xa2\x04\x00\x01\x10\n\x17\r\x1b\x04\x1f\t>\x04\x8a\x0e@\x04\x1d\x04C\n:\x0b\xad\x06\x89\x05\x19\x0b\x00\r\x05',
    "VIZIO_MENU": b'&\x00P\x00\x00\x01!\x98\x0e\x15\x0e\x14\x0f8\x11\x12\x11\x13\x0f\x14\x10\x13\x11\x12\x0f9\x108\x10\x12\x117\x108\x0f7\x117\x117\x107\x0f8\x107\x107\x11\x13\x10\x14\x0f7\x11\x12\x10\x13\x11\x14\x0f\x13\x12\x12\x106\x117\x10\x13\x116\x11\x00\x05:\x00\x01$K\x11\x00\r\x05',
    "VIZIO_ENTER": b'&\x00P\x00\x00\x01#\x94\x10\x13\x10\x13\x117\x10\x13\x11\x13\x0f\x13\x10\x13\x11\x13\x108\x124\x11\x13\x125\x108\x0f8\x108\x106\x11\x14\x0f\x13\x116\x10\x14\x13\x0f\x11\x12\x135\x11\x12\x117\x0f:\x10\x12\x108\x0e9\x108\x0e\x14\x126\x10\x00\x058\x00\x01$K\x11\x00\r\x05',
    "VIZIO_LEFT": b'&\x00P\x00\x00\x01#\x95\x10\x13\x11\x13\x107\x10\x13\x11\x12\x12\x11\x11\x13\x10\x13\x116\x107\x11\x13\x107\x107\x109\x107\x0f8\x117\x116\x108\x10\x14\x0f\x13\x11\x13\x106\x10\x13\x11\x13\x10\x13\x10\x13\x116\x108\x107\x11\x12\x116\x11\x00\x058\x00\x01$I\x12\x00\r\x05',
    "VIZIO_RIGHT": b'&\x00P\x00\x00\x01"\x95\x17\x12\x11\x13\x107\x10\x13\x10\x13\x11\x12\x11\x13\x10\x12\x118\x0f8\x11\x12\x107\x108\x107\x116\x117\x10\x13\x10\x13\x10\x13\x117\x0f\x14\x10\x13\x107\x11\x13\x107\x116\x108\x10\x13\x107\x108\x10\x13\x116\x10\x00\x05:\x00\x01$K\x0f\x00\r\x05',
    "VIZIO_EXIT": b'&\x00P\x00\x00\x01#\x95\x10\x13\x11\x12\x117\x11\x12\x10\x13\x10\x14\x11\x11\x11\x13\x107\x108\x0f\x13\x117\x117\x0f7\x117\x108\x0f8\x11\x12\x10\x13\x125\x10\x13\x11\x12\x137\x0f\x13\x10\x13\x109\x0f8\x0f\x15\x0e9\x0e8\x11\x13\x0e9\x10\x00\x058\x00\x01"M\x0f\x00\r\x05',
    "VIZIO_DISPLAY_LEFT": [
        "VIZIO_MENU",
        "VIZIO_ENTER",
        "VIZIO_LEFT",
        "VIZIO_EXIT",
    ],
    "VIZIO_DISPLAY_RIGHT": [
        "VIZIO_MENU",
        "VIZIO_ENTER",
        "VIZIO_RIGHT",
        "VIZIO_EXIT",
    ],
    "VIZIO_POWER": b'&\x00P\x00\x00\x01#\x94\x11\x14\x0f\x14\x0f9\x0e\x15\x0f\x15\r\x15\x0f\x14\x10\x13\x108\x0f7\x10\x15\x0f8\r9\x109\x0f8\x0f8\x0f\x14\x10\x13\x0f\x14\x0f:\x0e\x14\x0f\x13\x11\x14\x0f\x14\x0f7\x108\x108\x0f\x14\x0f7\x118\r;\x0e8\x10\x00\x05:\x00\x01"K\x10\x00\r\x05'
}

def press_button(device, name_spec, delay, directory):
    sequence = name_spec.split(",")
    if len(sequence) > 1:
        logging.info("pressing sequence of %s buttons", len(sequence))
        for button in sequence:
            press_button(device, button, delay, directory)
            time.sleep(delay)
        return
    m = re.match("(.*)x([0-9]+)$", name_spec)
    if m:
        name = m.group(1)
        repeat = int(m.group(2))
    else:
        name = name_spec
        repeat = 1
    logging.info("pressing button %s %s times", name, repeat)
    value = load_button(name, directory)
    assert value, f"no such button: {name}"
    if isinstance(value, list):
        buttons = value
        press_button(device, ",".join(buttons), delay, directory)
    elif isinstance(value, str):
        buttons = value
        press_button(device, buttons, delay, directory)
    elif isinstance(value, bytes):
        packet = value
        for _ in range(repeat):
            logging.info("pressing %s", name)
            device.auth()
            device.send_data(packet)
            time.sleep(delay)
    else:
        raise Exception(f"unknown type for button {name}")


def button_text_is_valid(button_text):
    return re.match("(([A-Z0-9_]+(x[0-9]+)?),?)+$", button_text)

def persist_button(name, packet_bytes, directory,
                   prompt_overwrites = False):
    filename = os.path.join(directory, name)
    logging.info("persisting button %s with %s bytes to %s",
                 name, len(packet_bytes), filename)
    if not os.path.exists(directory):
        os.mkdir(directory)
    assert os.path.exists(directory)
    if os.path.exists(filename):
        logging.warning("button with name %s already exists", name)
        if prompt_overwrites:
            input("confirm overwriting button %s...", name)
    with open(filename, "wb") as fh:
        fh.write(packet_bytes)
    logging.info("wrote %s bytes to %s", len(packet_bytes), filename)
    file_size = os.path.getsize(filename)
    logging.info("%s bytes in %s", file_size, filename)


def list_buttons(directory):
    logging.info("listing directory: %s", directory)
    for filename in os.listdir(directory):
        yield (filename, os.path.join(directory, filename))

def load_button(name, directory=None):
    if name in BUTTONS:
        return BUTTONS[name]
    buttons_map = dict(item for item in list_buttons(directory))
    if name in buttons_map:
        with open(buttons_map[name], "rb") as fh:
            data = fh.read()
            text = None
            try:
                text = data.decode().strip()
            except UnicodeDecodeError:
                pass
            if text and button_text_is_valid(text):
                return text
            return data
    raise KeyError("no such key: {name}")

class IRService(http.server.BaseHTTPRequestHandler):
    """
    REST API for a universal IR/RF remote.
    """

    def __init__(self, device, delay, directory):
        self.device = device
        self.delay = delay
        self.directory = directory

    def __call__(self, *args, **kwargs):
        # https://stackoverflow.com/a/58909293/1941755
        super(IRService, self).__init__(*args, **kwargs)


    def respond(self, status, body):
        """Sends an http response."""
        self.send_response(status)
        self.send_header("Content-type", "text/plain")
        self.end_headers()
        self.wfile.write(body.encode())

    def do_GET(self):
        """Handle GET requests."""
        try:
            buttons = self.path.strip("/")
            if not button_text_is_valid(buttons):
                logging.warning("unknown request: %s", self.path)
                self.respond(400, f"unknown route: {self.path}")
                return
            press_button(self.device, buttons, self.delay, self.directory)
            self.respond(200, "success!")
        except Exception as ex:
            logging.error("error during request handling: %s", ex)
            traceback.print_exc()
            ex_str = traceback.format_exc()
            self.send_response(500)
            contents = ex_str.encode()
            self.end_headers()
            self.wfile.write(contents)
            return

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--ip",
                        help="ip address of the broadlink IR remote",
                        default="192.168.1.40")
    parser.add_argument("-b", "--buttons", help="button name",
                        nargs="+")
    parser.add_argument("-p", "--port", help="port on which to listen",
                        type=int)
    parser.add_argument("-l", "--list", help="list all available buttons",
                        action="store_true")
    parser.add_argument("-L", "--learn", help="learn a new button",
                        action="store_true")
    parser.add_argument("-s", "--seconds",
                        help="delay in seconds between subsequent presses",
                        type=float, default=1)
    parser.add_argument("-d", "--directory",
                        help="root directory for persistent buttons",
                        default=os.path.expanduser("/home/ealfonso/afs/public/ir-buttons"))
    parser.add_argument("-i", "--interact", help="enter an interactive session",
                        action="store_true")
    args = parser.parse_args()

    logging.basicConfig(level=logging.DEBUG)

    if args.list:
        names = list(k for (k, _) in list_buttons(args.directory))
        names.extend(list(BUTTONS.keys()))
        print("\n".join(names))
        return

    logging.info("discovering at ip %s", args.ip)
    devices = broadlink.discover(discover_ip_address=args.ip)
    logging.info("found %s devices", len(devices))
    assert len(devices) > 0
    device = devices[0]

    if args.buttons:
        press_button(device, ",".join(args.buttons), args.seconds, args.directory)
    elif args.port:
        server_address = ('', args.port)
        httpd = http.server.HTTPServer(
            server_address,
            IRService(device=device, delay=args.seconds,
                      directory=args.directory))
        logging.info("serving on %s", server_address)
        httpd.serve_forever()
    elif args.learn:
        while True:
            device.auth()
            device.enter_learning()
            input("emit IR code, then press Return to continue...")
            packet = device.check_data()
            print(packet)
            # print(packet.hex())
            name = input("enter last button name: ")
            persist_button(name, packet, args.directory)
    elif args.interact:
        pdb.set_trace()
    else:
        parser.print_help(sys.stderr)
        raise Exception("no action specified")

if __name__ == "__main__":
    main()
