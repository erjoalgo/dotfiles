#!/bin/bash -x

if ! command -v firefox; then
    sudo apt-get install -y firefox-esr
fi

REPOS="${HOME}/git"
cd ${REPOS} || exit ${LIENO}

for REPO in \
    erjoalgo-firefox-addons \
    erjoalgo-vimfx-config \
    ;do

    test -d ${REPO} ||  \
	git clone "https://github.com/erjoalgo/${REPO}" || exit ${LINENO}

done

if ! test -d ~/.mozilla; then
    firefox &
    while ! test -d ~/.mozilla; do
	echo "waiting for ~/.mozilla creation..."
	sleep 2;
    done
fi

PROFILE=$(find ~/.mozilla/firefox -name '*.default' -type d)
if test -z "${PROFILE}"; then
    echo "unable to locate prfile directory" && exit ${LINENO}
fi
echo "using profile ${PROFILE}"
erjoalgo-firefox-addons/install-addons.sh "${PROFILE}"


PREFSJS=$(find ${PROFILE} -name prefs.js)
if test -z "${PREFSJS}"; then
    echo "unable to locate prefsjs" && exit ${LINENO}
fi

LINE=$(cat <<EOF
user_pref("extensions.VimFx.config_file_directory", "${HOME}/git/erjoalgo-vimfx-config/VimFx-config@vimfx.org/");
EOF
    )
if ! grep -F "${LINE}" "${PREFSJS}"; then
    # pkill firefox
    while ps -c | grep -v $$ | grep firefox; do
	echo "waiting for firefox exit to modify prefs.js ..."
	sleep 1
    done
    echo "${LINE}" >> "${PREFSJS}"
fi


# CHROME=$(find "${PROFILE}" -name chrome)
CHROME="${PROFILE}/chrome"
test -d "${CHROME}" || mkdir "${CHROME}"
ln -s "${HOME}/git/erjoalgo-vimfx-config/userChrome.css" \
   "${CHROME}"
