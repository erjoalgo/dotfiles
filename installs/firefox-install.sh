#!/bin/bash -x

set -euo pipefail

command -v firefox


which git-fetch-ff

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

EMACSMAIL_EXE=$(which emacs-mail)

# CHROME=$(find "${PROFILE}" -name chrome)
HANDLERS_JSON="${PROFILE}/handlers.json"
if ! test -e "${HANDLERS_JSON}"; then
    cat <<EOF > "${HANDLERS_JSON}"
{
  "defaultHandlersVersion": {},
  "mimeTypes": {
    "application/pdf": {
      "action": 3,
      "extensions": [
        "pdf"
      ]
    },
    "image/webp": {
      "action": 3,
      "extensions": [
        "webp"
      ]
    },
    "image/avif": {
      "action": 3,
      "extensions": [
        "avif"
      ]
    }
  },
  "schemes": {
    "mailto": {
      "stubEntry": true,
      "handlers": [
        null,
        {
          "name": "Gmail",
          "uriTemplate": "https://mail.google.com/mail/?extsrc=mailto&url=%s"
        }
      ]
    }
  },
  "isDownloadsImprovementsAlreadyMigrated": true,
  "isSVGXMLAlreadyMigrated": true
}
EOF
fi

# python3 -c /dev/stdin<<EOF
# import json
# with open('"${HANDLER_JSON}"', 'rw') as fh:
#   handlers = json.load(fh)
#   print(handlers)
#   handlers.setdefault('schemes', {})
#   handlers.schemes.setdefault('mailto', {})
#   handlers.schemes.mailto.setdefault('handlers', [])
#   handlers.schemes.mailto.handlers.push({name: "emacs-mail",
#     uriTemplate: "urn:scheme:externalApplication:mailto"})
# EOF

CHROME="${PROFILE}/chrome"
mkdir -p "${CHROME}"
ln -sf  "${VIMFX_CONFIG_DIR}/userChrome.css" "${CHROME}"
