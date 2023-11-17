#!/bin/bash -x

set -euo pipefail

ADB_CMD=adb

DEVICE_ID=$(adb shell settings get secure android_id)
DIR="${HOME}/Downloads/backups/${DEVICE_ID}"
mkdir -p "${DIR}"
cd "${DIR}"

if ! test -d sdcard; then
    adb pull /sdcard/. sdcard
fi

adb shell 'pm list packages' > pm-list-packages.txt

if ! test -s full-backup.ab; then
    adb backup -all -apk -obb -system
    test -e backup.ab
    mv backup.ab full-backup.ab
fi

test-port () {
    PROTOCOL=${PROTOCOL:-tcp};
    if test $# = 1; then
        HOST=localhost;
    else
        HOST="${1}" && shift;
    fi;
    PORT="${1}";
    exec 6<> /dev/${PROTOCOL}/${HOST}/${PORT}
}

if ! test-port 9222; then
    adb forward tcp:9222 localabstract:chrome_devtools_remote
fi

if ! test -e chrome-open-tabs.json; then
    curl  http://localhost:9222/json/list -Lo chrome-open-tabs.json
fi

adb logcat -d > logcat.txt


for APP in whatsapp signal telegram wechat authenticator; do
    read -p"confirm having backed up ${APP} to another phone: "
done

if ! test -e sms.xml -a -e calls.xml; then
    echo "missing {sms,calls}.xml" && exit ${LINENO}
fi

echo "success!"
