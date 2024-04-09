#!/usr/bin/env python3

import argparse
import fcntl
import glob
import json
import logging
import os
import re
import subprocess
import time
import traceback

class InputDetector(object):
    def __init__(self):
        self.fh_map = {}
        self.update_input_fh_map()

    @staticmethod
    def _set_nonblock(fh):
        orig_fl = fcntl.fcntl(fh, fcntl.F_GETFL)
        fcntl.fcntl(fh, fcntl.F_SETFL, orig_fl | os.O_NONBLOCK)


    def update_input_fh_map(self):
        for fname in glob.glob("/dev/input/by-path/*"):
            if fname not in self.fh_map:
                fh = open(fname, "rb")
                InputDetector._set_nonblock(fh)
                self.fh_map[fname] = fh

    def has_new_input(self):
        count = 0
        self.update_input_fh_map()
        for k in self.fh_map.keys()[:]:
            fh = self.fh_map[k]
            try:
                count += len(fh.read() or "")
            except OSError as ex:
                logging.warning(
                    "forgtting input %s which failed to read: %s",
                    k, ex)
                del self.fh_map[k]
        return count

class XInputDetector(object):
    def __init__(self):
        self.last_input = None

    def _read_xinput(self):
        cmd = ["timeout", "2", "xinput", "test-xi2", "--root"]
        p = subprocess.Popen(cmd,
                             stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
        stdout, stderr = p.communicate()
        if p.returncode != 124:
            raise subprocess.CalledProcessError(
                p.returncode, cmd, output=stdout, stderr=stderr)
        return stdout

    def has_new_input(self):
        curr_input = self._read_xinput()
        result = curr_input == self.last_input
        self.last_input = curr_input
        return result

def read_lid_state():
    lid_glob = "/proc/acpi/button/lid/LID*/state"
    fnames = glob.glob(lid_glob)
    if not fnames:
        raise Exception(f"no lid-state files found: {lid_glob}")
    with open(fnames[0], "r") as fh:
        text = fh.read()
        m = re.match("^state: +(.*)", text)
        if not m:
            raise Exception("failed to read lid state output: {text}")
        state = m.group(1)
        if state not in ["open", "closed"]:
            raise Exception("unknown lid state: {state}")
        return state

def read_battery_info():
    txt = subprocess.check_output(
        'upower -i "$(upower -e | grep battery)"', shell=True)
    info = {}
    for line in txt.decode().split("\n"):
        if not line.strip():
            continue
        items = line.split(":")
        if len(items) != 2:
            logging.debug(f"skipping non-key-value line: {line}")
            continue
        _key, val = items
        key = re.sub("[^a-zA-Z_]+", "", _key)
        info[key] = val
    return info

def loop(inter_sedation_cycles=20):
    last_activity = None
    counter = 0
    try:
        detector = XInputDetector()
        detector.has_new_input()
    except Exception:
        logging.warning("falling back to non-x input detector")
        detector = InputDetector()

    while True:
        time.sleep(60)
        now = time.time()
        if last_activity is None or detector.has_new_input():
            last_activity = now
        idle_secs = int(now - last_activity)
        try:
            lid_state = read_lid_state()
        except Exception as ex:
            logging.error("failed to read lid state: %s", ex)
            lid_state = None
        batteryInfo = read_battery_info()
        if counter % (1 + inter_sedation_cycles):
            continue
        try:
            custom_sedation(idle_secs, lid_state, batteryInfo)
        except Exception as ex:
            logging.error("failed to invoke custom sedation script: %s", ex)

def custom_sedation(idleSecs, lidState, batteryInfo):
    logging.info(f"idle: {idleSecs}s, lid: {lidState}")
    cmd = ["sedator", "-i", str(idleSecs)]
    if lidState is not None:
        cmd.extend(["-l", lidState])
    if batteryInfo and batteryInfo["present"] == "yes":
        cmd.extend(["-b", batteryInfo["percentage"]])
        cmd.extend(["-c", batteryInfo["state"]])
    retcode = subprocess.call(cmd)
    if retcode:
        raise subprocess.CalledProcessError(retcode, cmd)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-v", "--verbose",  action="store_true")
    args = parser.parse_args()
    if args.verbose:
        logging.basicConfig(level=logging.DEBUG)
    else:
        logging.basicConfig(level=logging.INFO)
    while True:
        try:
            loop()
        except Exception:
            traceback.print_exc()

if __name__ == "__main__":
    main()
