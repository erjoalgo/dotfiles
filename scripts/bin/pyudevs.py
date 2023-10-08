#!/usr/bin/env python3

from __future__ import absolute_import
import argparse
import asyncio
import logging
import os
import re
import subprocess
import threading
import time
import traceback

from six.moves import range
import pyudev

def x_service_curl(path, post_data=None, headers=None):
    script = "x-service-curl"
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


class DeviceHandler(object):
    locks = {}
    handlers = []
    max_retries = 5

    def __init__(self):
        self.last_triggered = None

    @staticmethod
    def matches(device):
        raise NotImplementedError()

    async def run(self):
        class_name = self.__class__.__name__
        logging.info("running handler for %s", class_name)
        self.__retry__(self.retry, name=class_name)


    @staticmethod
    def notify_info(text):
        logging.info(text)
        return notify_send(text)

    @staticmethod
    def notify_error(text):
        logging.info(text)
        return notify_send(text, color="red")

    @staticmethod
    def notify_success(text):
        logging.info(text)
        return notify_send(text, color="green")

    @classmethod
    def __retry__(cls, retriable_fn, delay_secs=2, name=""):
        error = None
        for retry in range(cls.max_retries):
            try:
              retriable_fn()
              DeviceHandler.notify_success("success: {}".format(name))
              return True
            except Exception as ex:
              logging.info("failed: %s", ex)
              time.sleep(delay_secs)
              error = ex
        assert error
        DeviceHandler.notify_error("{} failed: {}".format(name, str(error)))

    @staticmethod
    def check_call(cmd, name=""):
        ret = subprocess.call(cmd)
        if ret != 0:
          raise Exception(
              f"{name or str(cmd)} failed. exit status: {ret}")




class KeyboardHandler(DeviceHandler):

    def __init__(self, is_logitech=False, keyboard_id=None):
        self.is_logitech = is_logitech
        self.keyboard_id = keyboard_id

    @staticmethod
    def matches(device):
        vendor_product = "{}:{}".format(device.get("ID_VENDOR_ID"),
                                        device.get("ID_MODEL_ID"))
        devname = device.get("DEVNAME") or ""
        input_class = device.get(".INPUT_CLASS") or ""
        matches =  (devname
                    and "mouse" not in devname
                    and "mouse" not in input_class
                    and device.device_path.split("/")[-1].startswith("event"))
        # INFO:root:got new event: add Device('/sys/devices/virtual/misc/uhid/0005:04E8:7021.000A/input/input40/event18')
        m = re.search("(?<=/uhid/)([^/]+)[.]", device.device_path)
        if m:
            keyboard_id = m.group(1)
        else:
            keyboard_id = vendor_product

        if matches:
            is_logitech = device.get("ID_VENDOR") == "Logitech"
            logging.info(
                "got keyboard. vendor:product: %s, logitech? %s devname: %s input_class: %s",
                vendor_product, is_logitech, devname, input_class)
            return KeyboardHandler(is_logitech=is_logitech,
                                   keyboard_id=keyboard_id)

    def retry(self):
        filename = "xmodmap-load.sh"
        cmd = [filename]
        if self.is_logitech:
            cmd.push("-l")
        if self.keyboard_id:
              cmd.extend(["-i", self.keyboard_id])
        logging.info("running %s", " ".join(cmd))
        self.check_call(cmd)


DeviceHandler.handlers.append(KeyboardHandler())


class MonitorHandler(DeviceHandler):

    @staticmethod
    def matches(device):
        if device.get("SUBSYSTEM") == "drm":
            return MonitorHandler()

    def retry(self):
        self.notify_info("atempting to set up external monitor")
        ret = x_service_curl("/run", "correct-screen-newly-connected-displays")
        if ret:
          raise Exception("correct-screen failed: {}".format(ret))
        p = subprocess.Popen(["keynav-restart"],
                              stdout=subprocess.PIPE,
                              stderr=subprocess.PIPE)
        stdout, stderr = p.communicate()
        output = ((stdout or b"") + (stderr or b"")).decode()
        if p.returncode:
            logging.warn("keynav-restart failed: %s", output)


DeviceHandler.handlers.append(MonitorHandler())


class ScrcpyHandler(DeviceHandler):

    @staticmethod
    def matches(device):
        if device.action == "bind" and device.get("adb_user") == "yes":
            return ScrcpyHandler()

    def retry(self):
        script = "install-scrcpy-docker.sh"
        if not os.path.exists(script):
            logging.info("scrcpy not found: %s", script)
            return
        self.check_call([script])


# DeviceHandler.handlers.append(ScrcpyHandler())


class AdbTetheringHandler(DeviceHandler):

    max_retries = 1

    @staticmethod
    def matches(device):
        if device.action  == "bind" and device.get("adb_user") == "yes":
            return AdbTetheringHandler()

    def retry(self):
        script = "adb-start-tethering.sh"
        self.check_call([script])


# DeviceHandler.handlers.append(AdbTetheringHandler())


def monitor_forever():
    ctx = pyudev.Context()
    monitor = pyudev.Monitor.from_netlink(ctx)

    loop = asyncio.new_event_loop()
    threading.Thread(target=loop.run_forever).start()

    for device in iter(monitor.poll, None):
        # there might be a way to add the action condition
        # to the filter, but I couldn't find it
        logging.info("got new event: %s %s", device.action, device)
        if device.action in ("remove", "unbind"):
            logging.info("skipping remove event")
            continue
        if not device.is_initialized:
            logging.info("skipping non-initialized device")
            continue

        logging.debug("")
        for key in device.keys():
          logging.debug("%s: %s", key, device.get(key))
        logging.debug("")

        for match_handler in DeviceHandler.handlers:
            specific_handler = match_handler.matches(device)
            if specific_handler:
                logging.info("matched: %s", specific_handler)
                asyncio.run_coroutine_threadsafe(
                    specific_handler.run(), loop)
                break
        else:
            logging.info("nothing matched: %s", device)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-v", "--verbose",  action="store_true")

    args = parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.DEBUG)
    else:
        logging.basicConfig(level=logging.WARN)
    monitor_forever()
