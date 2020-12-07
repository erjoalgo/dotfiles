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


class DeviceHandler(object):
    locks = {}
    handlers = []

    def __init__(self):
        self.last_triggered = None

    def matches(self, device):
        raise NotImplementedError()

    async def run(self):
        class_name = self.__class__.__name__
        if self.last_triggered and (time.time() - self.lat_triggered)<1:
            logging.info("skipping multiple %s triggers within 1s", class_name)
            return
        self.last_triggered = time.time()
        with DeviceHandler.locks.get(class_name, threading.Lock()):
            logging.info("running handler for %s", class_name)
            # force only one handler at a time per class
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

    @staticmethod
    def __retry__(retriable_fn, max_retries=5, delay_secs=2, name=""):
        error = None
        for retry in range(max_retries):
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

    def matches(self, device):
        vendor_product = "{}:{}".format(device.get("ID_VENDOR_ID"),
                                        device.get("ID_MODEL_ID"))
        devname = device.get("DEVNAME")
        matches =  (device.get("ID_VENDOR") == "Logitech" and
                    devname and "mouse" not in devname
                    and device.device_path.split("/")[-1].startswith("event"))
        return matches

    def retry(self):
        self.notify_info("please touch any key on the keyboard...")
        filename = os.path.expanduser(
            "~/.stumpwmrc.d/scripts/bin/xmodmap-load.sh")
        logging.info("running xmodmap %s", filename)
        self.check_call([filename])


DeviceHandler.handlers.append(KeyboardHandler())


class MonitorHandler(DeviceHandler):

    def matches(self, device):
        return device.get("SUBSYSTEM") == "drm"

    def retry(self):
        self.notify_info("atempting to set up external monitor")
        ret = x_service_curl("/run", "correct-screen-no-prompt")
        if ret:
          raise Exception("correct-screen failed: {}".format(ret))


DeviceHandler.handlers.append(MonitorHandler())


class ScrcpyHandler(DeviceHandler):

    def matches(self, device):
        return device.action == "bind" and device.get("adb_user") == "yes"

    def retry(self):
        script = os.path.expanduser(
            "~/.stumpwmrc.d/scripts/installs/install-scrcpy-docker.sh")
        if not os.path.exists(script):
            logging.info("scrcpy not found: %s", script)
            return
        self.check_call([script])


DeviceHandler.handlers.append(ScrcpyHandler())


def monitor_forever():
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
            logging.info("skipping non-initialized device")
            continue

        logging.debug("")
        for key in device.keys():
          logging.debug("%s: %s", key, device.get(key))
        logging.debug("")

        for handler in DeviceHandler.handlers:
            if handler.matches(device):
                asyncio.run_coroutine_threadsafe(
                    handler.run(), loop)
                break
        else:
            logging.info("nothing matched: %s", device)

monitor_forever()
