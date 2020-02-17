#!/usr/bin/python
from enum import Enum
import argparse
import curses
import os
import pexpect
import signal
import sys
import thread
import time
import traceback

parser = argparse.ArgumentParser()
parser.add_argument("chars",
                    help = "a string of chars will be wrap with newlines when typed")
parser.add_argument("--pattern", "-p",
                    help = "enable wrapping only when this pexpect pattern is seen")
parser.add_argument("program", help = "the program and arguments to run", nargs="+")
args=parser.parse_args()

class GatingPatternState():
    def __init__(self):
        self.state = None

    def set_active(self, is_active):
        self.state = is_active

    def is_active(self):
        return bool(self.state)

def sigint_handler(signal, frame, child):
    # print ("DEBUG TRACE: wrap-single-char q8fo ")
    # import time; time.sleep(1)
    child.kill(signal)

def send_single_chars_loop(child, screen, chars, gating_pattern):
    while True:
        event = screen.getkey()
        if event in chars and gating_pattern.is_active():
            child.sendline(event)
            if gating_pattern:
                gating_pattern.set_active(False)
            if event == "e":
                curses.endwin()
        else:
            child.send(event)

def run(program, args, chars, pattern=None):
    gating_pattern_state = GatingPatternState() if pattern else None
    child = pexpect.spawn(program, args, encoding="utf-8")
    signal.signal(signal.SIGINT,
                  lambda signal, frame, child=child: sigint_handler(signal, frame, child))
    child.logfile_read = sys.stdout
    expect = [pexpect.EOF, pexpect.TIMEOUT]

    screen = curses.initscr()
    curses.noecho()
    thread.start_new_thread(send_single_chars_loop, (child, screen, chars, gating_pattern_state))

    if gating_pattern_state:
        expect.append(pattern)
        # expect.append(u".+")

    while True:
        index = child.expect(expect, timeout=None)
        if index == 0:
            break
        elif index == 1:
            continue
        elif index == 2 or index == 3:
            # print ("DEBUG wrap-single-char vdho: value of index: {}".format(index))
            # print ("DEBUG wrap-single-char e5av: value of child.before: {}".format(child.before))
            # print ("DEBUG wrap-single-char 8yi4: value of child.after: {}".format(child.after))
            assert gating_pattern_state
            if index == 2:
                gating_pattern_state.set_active(True)
                # curses.refresh()
            # print ("DEBUG wrap-single-char 6mpv: value of index: {}".format(index))
            # print ("DEBUG wrap-single-char qq0u: value of is_proxy_enabled(): {}".format(is_proxy_enabled()))
            # sys.stdout.write(child.before)
            # sys.stdout.write(child.after)
            sys.stdout.flush()
        else:
            assert(False)
    child.close()
    curses.endwin()
    exit(child.exitstatus)

if __name__ == "__main__":
    try:
        run(args.program[0], args.program[1:], chars=args.chars, pattern=unicode(args.pattern))
    except Exception as exc:
        traceback.print_exc(exc)
        try:
            curses.endwin()
        except:
            pass
        exit(1)
