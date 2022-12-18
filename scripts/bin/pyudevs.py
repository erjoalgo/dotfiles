#!/usr/bin/env python3

import asyncio
import logging
import os
import subprocess
import threading
import time

import pyudev

logging.basicConfig(level=logging.DEBUG)

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
              "failed: {}. exit status: {}".format(
                  name or str(cmd), ret))



class KeyboardHandler(DeviceHandler):

    def __init__(self, is_logitech=False):
        self.is_logitech = is_logitech

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
        if matches:
            is_logitech = device.get("ID_VENDOR") == "Logitech"
            return KeyboardHandler(is_logitech=is_logitech)

    def retry(self):
        self.notify_info("please touch any key on the keyboard...")
        filename = "xmodmap-load.sh"
        logging.info("running xmodmap %s", filename)
        cmd = [filename]
        if self.is_logitech:
            cmd.push(["-l"])
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
        p = subprocess.Popen(["keynav-restart"])
        stdout, stderr = p.communicate()
        if p.returncode:
            logging.warn("keynav-restart failed: %s %s", stdout, stderr)


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


DeviceHandler.handlers.append(AdbTetheringHandler())


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

        for handler in DeviceHandler.handlers:
            if handler.matches(device):
                logging.info("matched: %s", handler)
                asyncio.run_coroutine_threadsafe(
                    handler.run(), loop)
                break
        else:
            logging.info("nothing matched: %s", device)

monitor_forever()
