#!/usr/bin/env python3

import argparse
import glob
import json
import logging
import re
import subprocess
import time
import traceback

def read_xinput():
    cmd = ["timeout", "2", "xinput", "test-xi2", "--root"]
    p = subprocess.Popen(cmd,
                         stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE)
    stdout, stderr = p.communicate()
    if p.returncode != 124:
        raise subprocess.CalledProcessError(
            p.returncode, cmd, output=stdout, stderr=stderr)
    return stdout

def read_lid_state():
    lid_glob = "/proc/acpi/button/lid/LID*/state"
    fnames = glob.glob(lid_glob)
    if not fnames:
        raise Exception("no lid-state files found: {lid_glob}")
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
    last_input = read_xinput()
    counter = 0
    while True:
        # time.sleep(intervalSecs)
        currInput = read_xinput()
        now = time.time()
        if last_activity is None or currInput != last_input:
            last_activity = now
        last_input = currInput
        idleSecs = int(now - last_activity)
        lid_state = read_lid_state()
        batteryInfo = read_battery_info()
        if counter % inter_sedation_cycles:
            continue
        try:
            custom_sedation(idleSecs, lid_state, batteryInfo)
        except Exception as ex:
            logging.error("failed to invoke custom sedation script: %s", ex)

def custom_sedation(idleSecs, lidState, batteryInfo):
    logging.info(f"idle: {idleSecs}s, lid: {lidState}")
    cmd = ["sedator", "-i", str(idleSecs), "-l", lidState]
    if batteryInfo["present"] == "yes":
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
