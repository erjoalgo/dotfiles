#!/usr/bin/python3

import argparse
import random

parser = argparse.ArgumentParser()
parser.add_argument("-l", "--lower", default=1024)
parser.add_argument("-u", "--upper", default=49151)
parser.add_argument("-m", "--max_upper", action="store_true",
                    help="if specified, set upper to the maximum allowed port number")
args= parser.parse_args()

if args.max_upper:
    args.upper = 2**16-1

print(random.randint(args.lower, args.upper))
