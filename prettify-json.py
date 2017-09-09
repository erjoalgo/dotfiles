#!/usr/bin/python

import sys, json
json.dump(json.load(sys.stdin), sys.stdout, indent=2)
# sys.stdout.flush(sys.stdout)
