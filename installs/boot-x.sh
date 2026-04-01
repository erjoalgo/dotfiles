#!/bin/bash -x

set -euo pipefail

cd $( dirname "${BASH_SOURCE[0]}" )

sudo apt-get install -y dirmngr || true

./install-stumpwm.sh

./migrate-xinitrc.sh

EMACS_MAJOR_VERSION=$(emacs --version | head -1 | grep -Po '(?<=GNU Emacs )[^.]+') || true
if ! test "${EMACS_MAJOR_VERSION}" -le 28; then
  if ! ./emacs-install.sh; then
    echo "warning: failed to build emacs from source"
    which emacs || true
  fi
fi

sudo apt-get update
sudo apt-get install -y zathura konsole pass keynav at x2x
sudo apt-get install -y qimgv scrot

sudo DEBIAN_FRONTEND=noninteractive apt-get install -y wireless-tools wpasupplicant \
  macchanger expect iw net-tools
sudo apt-get install -y libxcomposite-dev libzstd-dev

./install-xsecurelock.sh

./install-find-cursor.sh || echo "WARNING: failed to install find-cursor"


if ! x-cursor-use.sh ../data/public/material_light_cursors; then
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

./konsole.sh

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
sudo apt-get install -y libgirepository-2.0-dev
python3 -m pip install solaar

./pyudevs.sh

mkdir -p ~/pictures/auto-scrots

sbcl --eval '(ql:quickload :erjoalgo-stumpwmrc)' --quit

sudo apt-get install -y redshift xcalib xbacklight xinput

# sudo apt-get install -y linphone linphone-cli

sudo apt-get install -y redshift

if ! which google-chrome chromium chrome; then
  sudo apt-get install -y chromium || sudo snap install chromium;
fi

# enable sleep, suspend, hibernate to avoid draining laptop battery
sudo systemctl unmask sleep.target suspend.target hibernate.target hybrid-sleep.target

./install-chrome-extensions.sh < ../data/public/chrome-extension-urls.txt

./chrome-disable-xdg-open-prompt.sh

./chrome-disable-bookmarks-bar.sh

./ir.sh

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
sudo apt-get install -y pipewire-jack pulseaudio-utils pavucontrol

sudo apt-get purge -y xdg-desktop-portal-gtk
sudo apt-mark hold xdg-desktop-portal-gtk
sudo apt-get install -y picom

insert-text-block \
    '# 31bbffac-8c44-41d3-b589-7f68c730ad5a-picom-backend'  \
                  ${HOME}/.config/picom.conf <<EOF
backend = "glx";
EOF


echo success
