#!/usr/bin/env python3

import asyncio
import logging
import pyudev
import subprocess
import threading
import time

logging.basicConfig(level=logging.DEBUG)

async def xmodmap():
    while True:
        logging.info("running xmodmap")
        time.sleep(2) # not sure if there's a race here, but it feels like there could be.
        ret = subprocess.call(["/home/ealfonso/.stumpwmrc.d/scripts/bin/xmodmap-load.sh"])
        if ret == 0:
            logging.info("success with xmodmap")
            break
        else:
            logging.info("return status: %s", ret)
    subprocess.call(["notify-send-stumpwm", "-m", "xmodmap success",
                     "-c", "green"])


def udev_monitor():
    ctx = pyudev.Context()
    monitor = pyudev.Monitor.from_netlink(ctx)
    monitor.filter_by("input")

    loop = asyncio.new_event_loop()
    threading.Thread(target=loop.run_forever).start()

    for device in iter(monitor.poll, None):
        # there might be a way to add the action condition to the filter, but I couldn't find it
        if device.action != "add":
            logging.info("skipping non-add action")
            continue
        elif not device.is_initialized:
            # ensure the KB is initialized -- not sure if this is actually a needed check
            logging.info("skipping non-initialized device")
            continue
        logging.info("got new device: %s", device)
        devname = device.get("DEVNAME")
        if not devname or "mouse" in devname:
            logging.info("skipping mouse")
            continue
        # my keyboard, from the output of `lsusb`
        vendor_product = "{}:{}".format(device.get("ID_VENDOR_ID"),
                                        device.get("ID_MODEL_ID"))


        if "046d:c52b" == vendor_product:
            for key in device.keys():
                logging.debug("%s: %s", key, device.get(key))
            # it's the keyboard being added.
            logging.info("detected adding logitech keyboard")
            for k in list(device.properties):
                logging.debug("%s: %s", k, device.get(k))
            logging.debug("device.tags: %s", list(device.tags))
            # import pdb;pdb.set_trace()
            asyncio.run_coroutine_threadsafe(xmodmap(), loop)

        time.sleep(1)

udev_monitor()
