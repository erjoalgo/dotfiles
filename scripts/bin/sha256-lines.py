#!/usr/bin/python

import sys, hashlib
for line in sys.stdin:
    contents = line.rstrip("\n")
    print(hashlib.sha256(contents).hexdigest())
