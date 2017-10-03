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
    PROFILE=$(find ~/.mozilla/firefox -maxdepth 1 -type d -iregex '.*/[a-z0-9]+[.].*')
    if test -z "${PROFILE}"; then
	echo "unable to locate profile directory" && exit ${LINENO}
    fi
fi
echo "using profile ${PROFILE}"
erjoalgo-firefox-addons/install-addons.sh "${PROFILE}"


PREFSJS=$(find ${PROFILE} -name prefs.js | head -1)
if test -z "${PREFSJS}"; then
    echo "unable to locate prefsjs" && exit ${LINENO}
fi

LINE=$(cat <<EOF
user_pref("extensions.VimFx.config_file_directory", "${HOME}/git/erjoalgo-vimfx-config/VimFx-config@vimfx.org");
EOF
    )
if ! grep -F "${LINE}" "${PREFSJS}"; then
    # pkill firefox
    while ps aux | grep -v "$$\|grep" | grep firefox ; do
	echo "waiting for firefox exit to modify prefs.js ..."
	sleep 1
    done
    echo "${LINE}" >> "${PREFSJS}"
fi


# add content type handlers
#   mailto => emacsmail

EMACSMAIL_EXE="${HOME}/git/erjoalgo-gnu-scripts/emacsmail"
xmlstarlet ed -L -S \
	   -a "/RDF:RDF/RDF:Description[last()]" -t elem -n "RDF:Description"  \
	   -a '$prev' -t attr -n RDF:about -v "urn:scheme:externalApplication:mailto" \
	   -a '$prev/..' -t attr -n NC:prettyName -v "emacsmail" \
	   -a '$prev/..' -t attr -n NC:path -v "${EMACSMAIL_EXE}" \
	   ${PROFILE}/mimeTypes.rdf

# CHROME=$(find "${PROFILE}" -name chrome)
CHROME="${PROFILE}/chrome"
test -d "${CHROME}" || mkdir "${CHROME}"
ln -sf "${HOME}/git/erjoalgo-vimfx-config/userChrome.css" \
   "${CHROME}"
