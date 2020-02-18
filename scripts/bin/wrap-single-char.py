#!/usr/bin/python
import argparse
import os
import pexpect
import re
import signal
import struct, fcntl, termios
import sys

class ProcFilter(object):
    def __init__(self, program, args, chars, pattern):
        self.pattern = re.compile(pattern) if pattern else None
        self.input_filter_enabled = not pattern
        self.chars = chars
        self.proc = None
        self.program = program
        self.args = args
        self.cum = ""

    def run(self):
        rows, columns = os.popen('stty size', 'r').read().split()
        self.proc = pexpect.spawn(self.program, self.args)
        self.proc.setwinsize(int(rows), int(columns))
        self.proc.interact(
            output_filter=lambda s, self=self: self.output_filter(s),
            input_filter=lambda s, self=self: self.input_filter(s)
            )
        self.proc.logfile_read = sys.stderr
        signal.signal(signal.SIGINT,
                      lambda signal, frame, self=self:
                      self.sigint_handler(signal, frame))
        signal.signal(signal.SIGWINCH,
                      lambda signal, frame, self=self:
                      self.sigwinch_passthrough(signal, frame))

    def input_filter(self, s):
        if (self.input_filter_enabled) and s in self.chars:
            s += "\n"
            if self.pattern:
                self.input_filter_enabled = False
        return s

    def output_filter(self, s):
        self.cum += s
        if self.pattern and self.pattern.search(self.cum):
            self.input_filter_enabled = True
            self.cum = ""
        return s

    def sigint_handler(self, signal, frame):
        self.proc.kill(signal)

    def sigwinch_passthrough(self, sig, data):
        s = struct.pack("HHHH", 0, 0, 0, 0)
        a = struct.unpack('hhhh', fcntl.ioctl(sys.stdout.fileno(),
            termios.TIOCGWINSZ , s))
        if not self.proc.closed:
            self.proc.setwinsize(a[0],a[1])

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("chars",
                        help = "a string of chars will be wrap with newlines when typed")
    parser.add_argument("--pattern", "-p",
                        help = "enable wrapping only when this pexpect pattern is seen")
    parser.add_argument("program", help = "the program and arguments to run", nargs="+")
    args=parser.parse_args()
    proc = ProcFilter(args.program[0],
                      args.program[1:],
                      chars = args.chars,
                      pattern = args.pattern)
    proc.run()
