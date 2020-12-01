#!/usr/bin/env python3

import asyncio
import getpass
import logging
import os
import pyudev
import subprocess
import threading
import time

logging.basicConfig(level=logging.DEBUG)

def x_service_curl(path, post_data=None, headers=None):
    script = os.path.expanduser(
              "~/.stumpwmrc.d/scripts/bin/x-service-curl")
    cmd = [script, path]
    if post_data:
        cmd.extend(["-d", post_data])
    for (k, v) in (headers or []):
        cmd.extend(["-H", k, v])
    return subprocess.call(cmd)


def notify_send(message, color=None):
  ret = x_service_curl(
      "/notify", post_data=message,
      headers=
      {"STUMPWM-MESSAGE-COLOR": color}
      if color else None)
  if ret:
    logging.error("failed to notify-send: %s", ret)


async def call_until_success(fn, timeout_secs=30):
    start = time.time()
    count = 0
    while count == 0 or (time.time() - start) < timeout_secs:
        try:
          fn()
          break
        except Exception as ex:
          logging.info("failed: %s", ex)
          time.sleep(1)
    assert ex
    notify_send(str(error_msg), color="red")


def configure_xmodmap():
    notify_send("please touch any key on the keyboard...")
    filename = os.path.expanduser(
        "~/.stumpwmrc.d/scripts/bin/xmodmap-load.sh")
    logging.info("running xmodmap %s", filename)
    ret = subprocess.call([filename])
    if ret == 0:
        logging.info("success with xmodmap")
        notify_send("success with xmodmap", color="green")
    else:
      raise Exception(
          "failed to set up keyboard layout with xmodmap. "
          "exit status: {}".format(ret))


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
            asyncio.run_coroutine_threadsafe(
                call_until_success(configure_xmodmap), loop)

udev_monitor()
