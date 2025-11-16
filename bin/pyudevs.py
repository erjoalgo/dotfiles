#!/usr/bin/env python3

import argparse
import asyncio
import logging
import os
import pyudev
import re
import subprocess
import threading
import time
import traceback

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
    delay_secs = 2

    def __init__(self):
        self.last_triggered = None
        self.desc = ""

    async def run(self):
        class_name = self.__class__.__name__
        logging.info("running handler for %s", class_name)
        error = None
        for _ in range(self.max_retries):
            try:
              self.retry()
              DeviceHandler.notify_success(f"success: {class_name} {self.desc}")
              return True
            except Exception as ex:
              logging.info("failed: %s", ex)
              time.sleep(self.delay_secs)
              error = ex
        DeviceHandler.notify_error("{} failed: {}".format(name, str(error)))


    def set_desc(self, desc):
        self.desc = desc

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

    @staticmethod
    def check_call(cmd, name=""):
        ret = subprocess.call(cmd)
        if ret != 0:
          raise Exception(
              f"{name or str(cmd)} failed. exit status: {ret}")




class KeyboardHandler(DeviceHandler):

    def __init__(self, device, keyboard_id=None):
        super().__init__()
        self.device = device
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
            logging.info(
                "got keyboard. vendor:product: %s, devname: %s input_class: %s, keyboard_id: %s",
                vendor_product, devname, input_class, keyboard_id)
            return KeyboardHandler(device, keyboard_id=keyboard_id)

    def retry(self):
        if self.device.get("ID_USB_MODEL") == "2.4G_Mouse":
            self.set_desc("media remote")
            script = os.path.expanduser("~/git/dotfiles/inits/.xmodmap/media-remote.sh")
            self.check_call([script])
            return

        is_logitech = self.device.get("ID_VENDOR") == "Logitech"
        logging.info(f"is logitech? {is_logitech}")
        filename = "xmodmap-load.sh"
        cmd = [filename]
        if is_logitech:
            self.set_desc("logitech")
            cmd.push("-l")
        if self.keyboard_id:
              cmd.extend(["-i", self.keyboard_id])
        logging.info("running %s", " ".join(cmd))
        self.check_call(cmd)


DeviceHandler.handlers.append(KeyboardHandler.matches)


class MonitorHandler(DeviceHandler):

    def __init__(self, device):
        self.device = device
        self.desc = ""

    @staticmethod
    def matches(device):
        if device.get("SUBSYSTEM") == "drm":
            return MonitorHandler(device)

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


DeviceHandler.handlers.append(MonitorHandler.matches)


class ScrcpyHandler(DeviceHandler):

    @staticmethod
    def matches(device):
        if device.action == "bind" and device.get("adb_user") == "yes":
            return ScrcpyHandler(device)

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

        logging.debug("device properties:")
        for key in device.keys():
          logging.debug("%s: %s", key, device.get(key))
        logging.debug("")

        for match_fn in DeviceHandler.handlers:
            specific_handler = match_fn(device)
            if specific_handler:
                logging.info(f"matched: {specific_handler} {specific_handler.desc}")
                asyncio.run_coroutine_threadsafe(specific_handler.run(), loop)
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
        logging.basicConfig(level=logging.INFO)
    while True:
        try:
            monitor_forever()
        except Exception:
            traceback.print_exc()
            time.sleep(5)
