#!/bin/bash -x

set -euo pipefail

cd $( dirname "${BASH_SOURCE[0]}" )

sudo apt-get install -y dirmngr || true

./installs/install-stumpwm.sh

if ! emacs --version | grep "26\\|27"; then
  if ! ./installs/emacs-install.sh; then
    echo "warning: failed to build emacs from source"
    which emacs || true
  fi
fi

sudo apt-get install -y zathura konsole pass keynav at x2x
sudo apt-get install -y eog scrot

sudo DEBIAN_FRONTEND=noninteractive apt-get install -y wireless-tools wpasupplicant \
  macchanger expect iw net-tools
sudo apt-get install -y libxcomposite-dev

./installs/install-xsecurelock.sh

./installs/install-find-cursor.sh ||
  echo "WARNING: failed to install find-cursor"

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

for DIR in ../lisp/{,cladaver} ~/git/{statusor,cl-voipms}; do
  test -d ${DIR}
  ASD=$(realpath $(echo "${DIR}/*asd"))
  SYSTEM=$(basename ${ASD} .asd)
  asdf-add-project-to-link-farm $(dirname "${ASD}")
  sbcl --eval "(ql:quickload :${SYSTEM})" --quit
  asdf-system-installed-p "${SYSTEM}"
done

# set up xdg-open configs
for XDG_OPEN_SCRIPT in ./installs/xdg-open-*; do
    ${XDG_OPEN_SCRIPT} || true
done

sudo apt-get install -y python3-pip
python3 -m pip install pyudev
# for the logitech wireless keyboard
sudo python3 -m pip install solaar

install-systemd-service.sh pyudevs <<EOF
[Unit]
Description=Run custom udev scripts via pyudev
Requires=systemd-udevd.service
After=systemd-udevd.service
StartLimitInterval=0

[Service]
ExecStart=$(pwd)/bin/pyudevs.py
User=$(whoami)
Restart=always
RestartSec=5
Environment=PATH=$PATH:$(pwd)/bin

[Install]
WantedBy=graphical.target

EOF

mkdir -p ~/pictures/auto-scrots

sbcl --eval '(ql:quickload :erjoalgo-stumpwmrc)' --quit

sudo apt-get install -y redshift xcalib xbacklight

sudo apt-get install -y linphone linphone-cli redshift

clone-git-repo https://github.com/erjoalgo/ledger-passwords-cli

insert-text-block '# 0d475bf3-8e6c-4ce3-a676-c07485f4fc5c-add-ledger-passwords-cli-path'  \
                  ${HOME}/.profile-env<<EOF
export PATH+=:${HOME}/git/ledger-passwords-cli/
EOF

${HOME}/git/ledger-passwords-cli/install.sh

if ! which google-chrome chromium chrome; then
  sudo apt-get install -y chromium || sudo snap install chromium;
fi


# enable sleep, suspend, hibernate to avoid draining laptop battery
sudo systemctl unmask sleep.target suspend.target hibernate.target hybrid-sleep.target

./installs/install-chrome-extensions.sh < ../data/public/chrome-extension-urls.txt

./installs/chrome-disable-xdg-open-prompt.sh

pip3 install chromeurl
chromeurl --install-manifest all

echo success
