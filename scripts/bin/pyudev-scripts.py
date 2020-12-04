#!/usr/bin/env python3

import asyncio
import logging
import os
import subprocess
import threading
import time

import pyudev

logging.basicConfig(level=logging.INFO)

def x_service_curl(path, post_data=None, headers=None):
    script = os.path.expanduser(
              "~/.stumpwmrc.d/scripts/bin/x-service-curl")
    cmd = [script, path]
    if post_data:
        cmd.extend(["-d", post_data])
    for (k, v) in (headers or {}).items():
        cmd.append("-H{}:{}".format(k, v))
    return subprocess.call(cmd)


def notify_send(message, color=None):
  ret = x_service_curl(
      "/notify", post_data=message,
      headers=
      {"STUMPWM-MESSAGE-COLOR": color}
      if color else None)
  if ret:
    logging.error("failed to notify-send: %s", ret)


async def call_until_success(fn, timeout_secs=30, _lock = threading.Lock()):
    with _lock:
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


def configure_monitor():
    notify_send("atempting to set up external monitor")
    ret = x_service_curl("/run",
                         "correct-screen-no-prompt")
    if ret:
      raise Exception("correct-screen failed: {}".format(ret))
    notify_send("successfully set up external monitor", color=green)


def udev_monitor():
    ctx = pyudev.Context()
    monitor = pyudev.Monitor.from_netlink(ctx)

    loop = asyncio.new_event_loop()
    threading.Thread(target=loop.run_forever).start()

    for device in iter(monitor.poll, None):
        # there might be a way to add the action condition to the filter, but I couldn't find it
        logging.info("got new event: %s %s", device.action, device)
        if device.action in ("remove", "unbind"):
            logging.info("skipping remove event")
            continue
        if not device.is_initialized:
            # ensure the KB is initialized -- not sure if this is actually a needed check
            logging.info("skipping non-initialized device")
            continue

        for key in device.keys():
          logging.debug("%s: %s", key, device.get(key))
        logging.debug("device.tags: %s", list(device.tags))

        vendor_product = "{}:{}".format(device.get("ID_VENDOR_ID"),
                                        device.get("ID_MODEL_ID"))
        devname = device.get("DEVNAME")
        LOGITECH_KEYBOARD_VENDOR_PRODUCT = "046d:c52b"
        if (LOGITECH_KEYBOARD_VENDOR_PRODUCT == vendor_product
            and devname and not "mouse" in devname
            and device.device_path.split("/")[-1].startswith("event")):
            logging.info("detected adding logitech keyboard")
            # import pdb;pdb.set_trace()
            asyncio.run_coroutine_threadsafe(
                call_until_success(configure_xmodmap), loop)
        elif device.get("SUBSYSTEM") == "drm":
          logging.info("detected adding or removing monitor")
          # a monitor
          asyncio.run_coroutine_threadsafe(
              call_until_success(configure_monitor), loop)
        else:
          continue

udev_monitor()
