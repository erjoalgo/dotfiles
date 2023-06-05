#!/usr/bin/python3

import lxml.html
import requests
import sys
import urllib.request as urllib

url = sys.argv[1]
page = urllib.urlopen(url)
t = lxml.html.parse(page)
print(t.find(".//title").text)
