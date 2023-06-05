#!/usr/bin/python3

import lxml.html
import requests
import sys
import urllib.request as urllib

url = sys.argv[1]
page = urllib.urlopen(url)
t = lxml.html.parse(page)
print(t.find(".//title").text)


"""from mechanize import Browser
br = Browser()
br.open("http://www.google.com/")
print(br.title())
"""

"""
from BeautifulSoup import BeautifulSoup
soup = BeautifulSoup(urllib2.urlopen())
print(soup.title.string)
"""


"""#!/bin/bash

set -euo pipefail

URL=${1} && shift

if ! command -v hxselect; then
   sudo apt-get install -y html-xml-utils libxml2-utils
fi

wget -qO- "${URL}" | hxselect -s '\n' -c  'title' 2>/dev/null
"""
