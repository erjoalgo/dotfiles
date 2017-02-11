#!/bin/bash -x

FIREFOX_NEW_TAB=$(which firefox-new-tab.sh)
test $? -eq 0 || exit ${LINENO}

sudo update-alternatives --install $(which x-www-browser) \
     x-www-browser ${FIREFOX_NEW_TAB} 200

sudo update-alternatives --set x-www-browser ${FIREFOX_NEW_TAB}
