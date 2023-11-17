#!/usr/bin/python

from __future__ import absolute_import
from __future__ import print_function
import sys, hashlib
for line in sys.stdin:
    contents = line.rstrip("\n")
    print((hashlib.sha256(contents).hexdigest()))
