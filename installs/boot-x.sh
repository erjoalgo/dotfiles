#!/bin/bash -x

set -euo pipefail

cd $( dirname "${BASH_SOURCE[0]}" )

sudo apt-get install -y dirmngr || true

./install-stumpwm.sh

EMACS_MAJOR_VERSION=$(emacs --version | head -1 | grep -Po '(?<=GNU Emacs )[^.]+') || true
if ! test "${EMACS_MAJOR_VERSION}" -le 28; then
  if ! ./emacs-install.sh; then
    echo "warning: failed to build emacs from source"
    which emacs || true
  fi
fi

sudo apt-get install -y zathura konsole pass keynav at x2x
sudo apt-get install -y eog scrot

sudo DEBIAN_FRONTEND=noninteractive apt-get install -y wireless-tools wpasupplicant \
  macchanger expect iw net-tools
sudo apt-get install -y libxcomposite-dev

./install-xsecurelock.sh

./install-find-cursor.sh || echo "WARNING: failed to install find-cursor"

if ! ./x-cursor.sh; then
    echo "warning: failed to install custom x-cursor"
fi

# https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=856351
sudo insert-text-block '# 37561c4f-5b87-4252-9724-6eed90ee3943-fix-stretch-X-issue'  \
                  /etc/X11/Xwrapper.config<<EOF
needs_root_rights=yes
EOF

which update-config-file-key-value

sudo $(which update-config-file-key-value) \
  -f /etc/systemd/logind.conf  \
  -k HandlePowerKey -v ignore

KONSOLERC=${HOME}/.config/konsolerc
if test -e "$KONSOLERC"; then
    sed -i '/^DefaultProfile=/d' ${KONSOLERC}
fi

insert-text-block '# eab944d5-9973-4f44-b2e0-1b168f164397-konsolerc-defaults'  \
                  ${KONSOLERC} -b << EOF
# konsolerc file tends to be managed by konsole in a site-specific way.
[Desktop Entry]
DefaultProfile=erjoalgo.profile

[Favorite Profiles]
Favorites=

[KonsoleWindow]
ShowMenuBarByDefault=false
EOF

function clone-git-repo {
    URL=${1} && shift
    NAME=$(basename "${URL}")
    DIR=${HOME}/git/${NAME}
    if ! test -d "${DIR}"; then
        git clone "${URL}" "${DIR}"
    fi
    pushd .
    cd "${DIR}"
    git pull --ff-only
    popd
}

for URL in \
  https://github.com/usocket/usocket \
  https://github.com/erjoalgo/{erjoalgo-webutil,cl-voipms,statusor} \
  ; do
    clone-git-repo "${URL}"
    quicklisp-register-local-project "${DIR}"
done

find ~/.cache/common-lisp/ -path '*dotfiles/lisp/*.fasl' -exec rm {} +

sbcl --eval "(ql:update-all-dists :prompt nil)" --quit

for DIR in ~/git/{statusor,cl-voipms} ../lisp/{cladaver,} ; do
  test -d ${DIR}
  ASD=$(realpath $(echo "${DIR}/*asd"))
  SYSTEM=$(basename ${ASD} .asd)
  asdf-add-project-to-link-farm $(dirname "${ASD}")
  sbcl --eval "(ql:quickload :${SYSTEM})" --quit
  asdf-system-installed-p "${SYSTEM}"
done

# set up xdg-open configs
for XDG_OPEN_SCRIPT in ./xdg-open-*; do
    ${XDG_OPEN_SCRIPT} || true
done

sudo apt-get install -y python3-pip

# for the logitech wireless keyboard
python3 -m pip install solaar

./pyudevs.sh

mkdir -p ~/pictures/auto-scrots

sbcl --eval '(ql:quickload :erjoalgo-stumpwmrc)' --quit

sudo apt-get install -y redshift xcalib xbacklight xinput

sudo apt-get install -y linphone linphone-cli redshift

clone-git-repo https://github.com/erjoalgo/ledger-passwords-cli

insert-text-block '# 0d475bf3-8e6c-4ce3-a676-c07485f4fc5c-add-ledger-passwords-cli-path'  \
                  ${HOME}/.profile-env<<EOF
export PATH+=:${HOME}/git/ledger-passwords-cli/
EOF

${HOME}/git/ledger-passwords-cli/install.sh || true

if ! which google-chrome chromium chrome; then
  sudo apt-get install -y chromium || sudo snap install chromium;
fi

# enable sleep, suspend, hibernate to avoid draining laptop battery
sudo systemctl unmask sleep.target suspend.target hibernate.target hybrid-sleep.target

./install-chrome-extensions.sh < ../data/public/chrome-extension-urls.txt

./chrome-disable-xdg-open-prompt.sh

clone-git-repo "https://github.com/erjoalgo/chromeurl"
pushd .
cd ~/git/chromeurl/native
source ~/.venv/bin/activate
pip3 install -U .
for _ in $(seq 2); do
  if chromeurl --install-manifest all; then
    break
  fi
  mkdir ~/.config/chromium
done
popd

sudo apt-get purge -y pipewire-pulse
sudo apt-get install -y pipewire-jack

echo success
