#!/usr/bin/python
import argparse
import os
import pexpect
import re
import signal
import sys

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

def beep(freq=None):
    os.system("beep.sh {}".format(freq or ""))

def endwin():
    try:
        beep()
    except:
        pass
    try:
        curses.endwin()
    except:
        # traceback.print_exc()
        pass

def sigint_handler(signal, frame, child):
    child.kill(signal)
    endwin()

def send_single_chars_loop(child, screen, chars, gating_pattern):
    while True:
        event = screen.getkey()
        if event in chars and gating_pattern.is_active():
            if gating_pattern:
                gating_pattern.set_active(False)
            if event == "e":
                endwin()
            child.sendline(event)
        else:
            try:
                # may fail if child was already closed
                child.send(event)
            except:
                pass

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

    while True:
        index = child.expect(expect, timeout=None)
        if index == 0:
            break
        elif index == 1:
            continue
        elif index == 2 or index == 3:
            assert gating_pattern_state
            if index == 2:
                beep("-f880")
                gating_pattern_state.set_active(True)
            sys.stdout.flush()
        else:
            assert(False)
    child.close()
    endwin()
    exit(child.exitstatus)

if __name__ == "__main__":
    try:
        run(args.program[0], args.program[1:], chars=args.chars, pattern=unicode(args.pattern))
    except Exception as exc:
        traceback.print_exc(exc)
        endwin()
        exit(1)
