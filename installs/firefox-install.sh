#!/bin/bash -x

set -euo pipefail

command -v firefox


sudo apt-get install -y xmlstarlet

which git-fetch-ff
which xmlstarlet

GIT_HOME="${HOME}/git" git-fetch-ff \
    "https://github.com/erjoalgo/erjoalgo-firefox-addons" \
    "https://github.com/erjoalgo/erjoalgo-vimfx-config"

test -d ~/.mozilla

# find profile
PROFILE=${PROFILE:-$(find ~/.mozilla/firefox -name '*.default' -type d | head -1)}
if test -z "${PROFILE}"; then
    PROFILE=$(find ~/.mozilla/firefox -maxdepth 1 -type d -iregex '.*/[a-z0-9]+[.].*' \
	 | head -1)
    if test -z "${PROFILE}"; then
	echo "unable to locate profile directory" && exit ${LINENO}
    fi
fi
echo "using profile ${PROFILE}"
${HOME}/git/erjoalgo-firefox-addons/install-addons.sh "${PROFILE}"


PREFSJS=$(find "${PROFILE}" -name prefs.js | head -1)
if ! test -f "${PREFSJS}"; then
    PREFSJS="${PROFILE}/prefs.js"
    touch "${PREFSJS}"
fi


while pidof firefox; do
    echo "waiting for firefox exit to modify prefs.js ..." && sleep 1
done

VIMFX_CONFIG_DIR="${HOME}/git/erjoalgo-vimfx-config"
sleep 10 # ensure changes don't get overwritten
insert-text-block '// fd5c00d3-7d91-4dd3-933c-9d8e590f1e51-add-vimfx-config-file-directory' "${PREFSJS}"<<EOF
user_pref("extensions.VimFx.config_file_directory", "${VIMFX_CONFIG_DIR}/VimFx-config@vimfx.org");
EOF

insert-text-block '# ee72ded3-b0d4-4804-9348-44f0f5e01e14-tree-style-tab-prefs'  \
		  "${PREFSJS}" <<EOF
user_pref("extensions.treestyletab.closeParentBehavior", 2);
EOF

insert-text-block '# ee72ded3-b0d4-4804-9348-44f0f5e01e14-tree-style-tab-prefs'  \
		  "${PREFSJS}" <<EOF
user_pref("layout.css.devPixelsPerPx", 3);
EOF


# add content type handlers
#   mailto => emacsmail

EMACSMAIL_EXE=$(which emacsmail)

xmlstarlet ed -L -S \
	   -a "/RDF:RDF/RDF:Description[last()]" -t elem -n "RDF:Description"  \
	   -a '$prev' -t attr -n RDF:about -v "urn:scheme:externalApplication:mailto" \
	   -a '$prev/..' -t attr -n NC:prettyName -v "emacsmail" \
	   -a '$prev/..' -t attr -n NC:path -v "${EMACSMAIL_EXE}" \
	   "${PROFILE}/mimeTypes.rdf"

# CHROME=$(find "${PROFILE}" -name chrome)
CHROME="${PROFILE}/chrome"
mkdir -p "${CHROME}"
ln -sf  "${VIMFX_CONFIG_DIR}/userChrome.css" "${CHROME}"
