#!/usr/bin/python3

from __future__ import absolute_import
from __future__ import print_function
import argparse
import base64
import json
import subprocess

parser = argparse.ArgumentParser()
parser.add_argument("config", help = "path to config file")
parser.add_argument("out", help = "path to output qr code")

args=parser.parse_args()

config=json.loads(open(args.config).read())

url_noscheme="{}:{}@{}:{}".format(*(config[prop] for prop in
             ("method", "password", "server", "server_port")))

print(("plaintext url: {}".format(url_noscheme)))
encoded="ss://{}".format(base64.b64encode(url_noscheme.encode()))

print(("base64 encoded: {}".format(encoded)))
retcode=subprocess.call(["qrencode", "-o", args.out, encoded], stdin=subprocess.PIPE)

if retcode==0:
    print(("wrote to {}".format(args.out)))
else:
    print(("non-zero exit code: {}".format(retcode)))
