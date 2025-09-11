#!/bin/bash

set -xeuo pipefail

while getopts "h0" OPT; do
    case ${OPT} in
    0)
        LOWBANDWITH=true
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done


APT_GET=$(which apt-get yum 2>/dev/null) || true
if which sudo && SUDO_ASKPASS=$(which false) sudo true; then
  SUDOCMD="sudo bash -c"
else
  SUDOCMD="su -c"
fi

function maybe-install-sudo {
    if ! which sudo && test -n "${APT_GET}"; then
        ${SUDOCMD} "${APT_GET} install -y sudo" || true
        ${SUDOCMD} "${APT_GET} install -y ntp" || true
    fi
}

maybe-install-sudo

cd

if which apt-get; then
    if true || ! ${SUDOCMD} "apt-get update" ||  \
        ${SUDOCMD} "grep ^deb\ cdrom /etc/apt/sources.list"; then
        DOTFILES_GITHUB_URL=https://raw.githubusercontent.com/erjoalgo/dotfiles/master/
        URL=${DOTFILES_GITHUB_URL}/installs/update-sources-list.sh
        BASE=$(basename ${URL})
        pushd .
        cd /tmp
        if ! test -e ${BASE}; then
            if which curl; then
                curl "${URL}" -Lo "${BASE}"
            else
                wget "${URL}" -O "${BASE}"
            fi
            chmod +x ${BASE}
        fi
        ${SUDOCMD} ./${BASE}
        ${SUDOCMD} "apt-get update"
        popd
    fi
    if ! which sudo; then
        ${SUDOCMD} "apt-get install -y sudo"
        ${SUDOCMD} "apt-get install -y ntp" || true
    fi
fi

maybe-install-sudo

# set up passwordless sudo
NOPASSWD_LINE="${USER} ALL=(ALL:ALL) NOPASSWD:ALL"
if test -d /etc/sudoers.d; then
    # use sudoers.d if possible
    ${SUDOCMD} "tee /etc/sudoers.d/${USER}-nopasswd <<< \"${NOPASSWD_LINE}\""
    # INCLUDE_SUDOERS_LINE="#includedir /etc/sudoers.d"
elif ! ${SUDOCMD} "grep -F \"${LINE}\" /etc/sudoers"; then
    ${SUDOCMD} "tee -a /etc/sudoers <<< \"${NOPASSWD_LINE}\""
fi

sudo echo "successful passwordless sudo"

sudo apt-get update

function sagiy {
    for _ in $(seq 3); do
        sudo apt-get install -y ${*} && break
        sudo apt-get update --fix-missing || true
        sleep 5
    done
}

sagiy git curl python3 python3-full

GIT_HOME=${HOME}/git

# backwards compatibility
ERJOALGO_STUMPWMRC=${GIT_HOME}/erjoalgo-stumpwmrc
DOTFILES=${GIT_HOME}/dotfiles
if test -e "${ERJOALGO_STUMPWMRC}" -a ! -e "${DOTFILES}"; then
  mv "${ERJOALGO_STUMPWMRC}" "${DOTFILES}"
  ln -s "${DOTFILES}" "${ERJOALGO_STUMPWMRC}"
fi

function fetch-repos  {
  # fetch my git repos
  GIT_EMAIL=erjoalgo@gmail.com
  GIT_NAME="Ernesto Alfonso"
  mkdir -p ${GIT_HOME}
  pushd .
  for REPO in ${*}; do
    cd ${GIT_HOME}
    test -d ${REPO} || git clone "https://github.com/erjoalgo/${REPO}"
    cd ${REPO}
    git config user.name "${GIT_NAME}"
    git config user.email "${GIT_EMAIL}"
    for TRIES in $(seq 5); do
        if git pull --ff-only; then
            break
        fi
        echo "git pull failed, possible network issue. retrying..."
        sleep 1
    done
  done
  popd
}

sed -i 's/${RANDOM}/{RANDOM}/' ~/.ssh/config || true

fetch-repos dotfiles

cd ~/git/dotfiles/installs/

git submodule update --init --recursive

SCRIPTS_BIN="${HOME}/git/dotfiles/bin"
test -d "${SCRIPTS_BIN}"
export PATH=$PATH:${SCRIPTS_BIN}

which insert-text-block
sudo ln -fs ${SCRIPTS_BIN}/insert-text-block /usr/bin

insert-text-block '# 58b63bb1-de24-449c-9bf9-9f317a28b9ac-load-scripts-bin'  \
                  ${HOME}/.profile-env <<"EOF"
PATH=${PATH}:${HOME}/git/dotfiles/bin/
EOF

sudo insert-text-block \
  '# ac25d55a-723b-4da2-bfa6-674c51e7eefb-max-out-virtual-terminal-size' \
  /etc/default/console-setup<<EOF
FONTFACE="TerminusBold"
FONTSIZE="16x32"
EOF

sudo sed -i 's/^XKBOPTIONS=/# \0/g' /etc/default/keyboard || true

sudo insert-text-block  \
  '# 81475907-8f33-420a-b002-118c8d4a62ae-configure-console-keyboard'  \
  /etc/default/keyboard <<EOF
XKBOPTIONS="terminate:ctrl_alt_bksp,ctrl:nocaps"
EOF

sudo service console-setup restart || true

if test "${BOOT:-}" = wifi; then
    ./wifi-boot.sh
    echo "completed wifi install"
    exit 0
fi

if test 0 -eq "${EUID}"; then
  echo "do not run this script as root"
fi

sudo ${APT_GET} install -y python3-pip vim python-is-python3

python3 -m venv ~/.venv
source ~/.venv/bin/activate

pip3 install getchwrap -U || true
pip install requests
pip install git+https://gitlab.com/thelabnyc/requests-unixsocket2

./link-inits.sh

for SCRIPT in  \
        ./gen-git-config.sh \
    ;do
    ./${SCRIPT} || true
done

# set default cmd line editor to vi
sudo update-alternatives --set editor /usr/bin/vim --verbose || true

for LINK in /usr/bin/{emacsclient,editor}; do
    WRAPPER=${HOME}/.stumpwmrc.d/bin/emacsclient-wrapper.sh
    NAME=$(basename "${LINK}")
    sudo update-alternatives --install "${LINK}" "${NAME}" "${WRAPPER}" 0
    sudo update-alternatives --set "${NAME}" "${WRAPPER}" --verbose
done



sudo insert-text-block  \
     '"xUxm084v1yfXHaLwwMFubYIub1eOsvKo-system-wide-vi-settings"'  \
     /usr/share/vim/vimrc \
    < ${HOME}/.vimrc

sudo ln -sf ${HOME}/.vimrc /etc/vim/vimrc.local
sudo ln -sf ${HOME}/.vimrc ~root/.vimrc


sudo debconf-set-selections <<EOF
debconf frontend stromg Editor
EOF

insert-text-block '# bbdede6e-87c5-4ba9-927e-78865afb3dcb-source-my-bashrc'  \
		  ${HOME}/.bashrc <<EOF
test -n "\${DEBUG_INIT:-}" && echo "loading \$BASH_SOURCE (uzvr)"
source ${HOME}/.my-bashrc
EOF

insert-text-block '# jPC5VOJsRIpcLjXh9o0mJMgPknbejdjl-source-my-bash-profile'  \
                  "${HOME}/.bash_profile" <<EOF
test -n "\${DEBUG_INIT:-}" && echo "loading \$BASH_SOURCE (hWc1)"
source ${HOME}/.my-bash-profile
EOF

XSESSIONRC=${HOME}/.xsessionrc
touch ${XSESSIONRC}
chmod +x ${XSESSIONRC}
# lightdm does not source ~/.profile
# https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=636108
for SHADOWER in ~/.profile  \
                    ~/.bash_profile \
                    ~/.bash_login \
                    ${XSESSIONRC} \
                    ~/.bashrc \
                ; do
    if test -e $SHADOWER; then
	insert-text-block '# 5a82826a-aad9-11e7-872b-4fada3489c57-source-my-profile'  \
		          -b ${SHADOWER}<<EOF
test -n "\${DEBUG_INIT:-}" && echo "loading \$BASH_SOURCE (JvtE)"
. ${HOME}/.my-profile
EOF
    fi
done

# remove any limits on bash history
sed -i '/^HIST\(FILE\)\?SIZE=[0-9]*/d' "${HOME}/.bashrc" || true

# set GRUB timeout to zero
GRUB_FILE=/etc/default/grub
if test -e ${GRUB_FILE} &&  \
	grep -P '^GRUB_TIMEOUT=[0-9]+$' ${GRUB_FILE} | grep -v '=0'; then
    sudo sed -i 's/^GRUB_TIMEOUT=[0-9]*$/GRUB_TIMEOUT=0/' ${GRUB_FILE}
    which update-grub && sudo update-grub
fi

mkdir -p ${HOME}/src

if test "${BOOT:-}" = basic; then
    echo "completed basic install"
    exit 0
fi

fetch-repos dotemacs githost autobuild tmux-session-spectrum

# some essential tools
if test -n "${APT_GET}"; then
    sudo ${APT_GET} install -y htop fail2ban unison tmux wget colordiff netcat-openbsd \
       apt-file unattended-upgrades nmap bash-completion man-db rsyslog ufw etherwake
    sudo ${APT_GET} install -y bootlogd || true
    # auditd may fail with "audit support not in kernel"
    if ! sudo ${APT_GET} install -y auditd; then
	sudo ${APT_GET} purge -y auditd
    fi
fi

insert-text-block '# 91352955-c448-4c16-a4d4-54470089c900-notify-lagging-repos-user-crontab' \
    <(crontab -l 2>/dev/null) -o >(crontab) <<EOF
30 10 * * * bash -c '${SCRIPTS_BIN}/git-notify-lagging-repos.sh ~/git/* 2>&1 > ~/.git-lagging-repos-report'
EOF

mkdir -p ${HOME}/.ssh
insert-text-block '# 99ef88b9-660b-458d-9dfd-9cf090778ea5-include-private-ssh-config' \
                  ${HOME}/.ssh/config -b <<EOF
Include ~/private-data/configs/ssh-config
EOF

cat <<EOF | tee -a ~/.ssh/authorized_keys
ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBAyqPpjzbRuF5Nxt4QAQF7IwA7IwpoQIFLYYLE1WQ/h1TwLYwxhoPjSewCmGFc8qO4afc9IkQGzJSsdRdLCNR20= <ssh://ealfonso|nist256p1>
EOF

chmod 600 ~/.ssh/config

SSH_PRIVATE_KEY_PATH=~/.ssh/id_rsa
if ! test -e ${SSH_PRIVATE_KEY_PATH}; then
    ssh-keygen -t rsa -b 4096 -o -a 100 -N '' -f ${SSH_PRIVATE_KEY_PATH}
fi

touch ~/.ssh/authorized_keys
chmod 600 ~/.ssh/authorized_keys

insert-text-block '# 02a8943a-e369-47f7-9e96-11de241c2e36-add-universal-ssh' \
                  ~/.ssh/authorized_keys<<EOF
ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBHgs/j9IDFtOS8FriMd8ddZziQoNbZllfeobTqSeClJyimUiiSm+uiqP/0mL4FC2HZmeeRKVlGJx8DUQWUBHVPE= <ssh://erjoalgo@gmail.com|nist256p1>
EOF

insert-text-block -b '# 0c26fa16-1154-4554-8dbd-056097fb58d0-add-emacs-mode' \
                  ${HOME}/.profile-env <<EOF
# -*- mode: sh -*-
EOF

insert-text-block '# 9f56be79-fa85-4ec0-ab4f-a9d3df5fef76-maybe-add-gopath' ~/.profile-env  \
  <<EOF
if test -d ~/go/bin/; then
   export PATH=\$PATH:${HOME}/go/bin/
fi
EOF

insert-text-block '# 20bf6fe5-1525-4fba-905a-beec9b3df1e5-add-usr-games-to-path' \
  ~/.profile-env  \
  <<EOF
export PATH=\$PATH:/usr/games/
EOF

insert-text-block '# 59a39cbe-5416-42b7-b66e-43227eb01227-add-server-configs-bin'  \
                  ~/.profile-env<<EOF
export PATH=\$PATH:${HOME}/git/server-configs/bin
EOF

insert-text-block '# 8c65cfa8-bc9f-414c-a8d2-de367865a539-add-emacs-scripts'   \
                  ~/.profile-env<<EOF
export PATH=\$PATH:${HOME}/git/dotemacs/lisp/bin
EOF

insert-text-block '# a1ec5b17-3ee5-4397-9d74-1094e51c0975-source-static-profile-env'  \
                  ~/.profile-env<<EOF
source ${HOME}/git/dotfiles/inits/.profile-env-static
EOF

insert-text-block ';; 5ef52c11-e976-4eb5-90fa-38795231059d-load-my-sbclrc' \
   ${HOME}/.sbclrc <<EOF
  ;; #-dbg
  (let ((my-sbclrc (merge-pathnames "git/dotfiles/inits/.my-sbclrc"
                                         (user-homedir-pathname))))
    (when (probe-file my-sbclrc)
      (load my-sbclrc)))
EOF

X_BROWSER=$(which x-www-browser-stumpwm)

sudo ${APT_GET} install -y openssh-server

if test -e /usr/bin/ssh.debian || ./ssh-random-env; then
    sed -i 's/-{RANDOM}/-${RANDOM}/' ~/.ssh/config || true
fi

insert-text-block "# 391f301e-c328-43f2-84ff-94de868293c7-ssh-send-env-desktop-group-number"  \
  "${HOME}/.ssh/config" << EOF
Match host *
    SendEnv DESKTOP_GROUP_NUMBER
    RemoteForward /tmp/.x-service-$(hostname)-\${RANDOM}.sock localhost:1959
EOF

SSHD_CONFIG=/etc/ssh/sshd_config
if test -f "${SSHD_CONFIG}"; then
    sudo sed -i 's/PasswordAuthentication/# &/g' "${SSHD_CONFIG}"
    sudo insert-text-block  \
    '# 59b01e7b-7982-4fc0-8b3d-6a7b7cb43ca0-ssh-accept-env-desktop-group-number' \
    "${SSHD_CONFIG}"  \
    <<EOF
PasswordAuthentication no
AcceptEnv DESKTOP_GROUP_NUMBER
AcceptEnv DEBUG_INIT
# https://unix.stackexchange.com/questions/427189/
StreamLocalBindUnlink yes
EOF
fi

sudo insert-text-block '# Zss7UaEgcFtP1T8JPS7h77vOaQlYDR3H-enable-virtual-terminals' \
     /etc/systemd/logind.conf <<EOF
NAutoVTs=6
ReserveVT=6
EOF

sudo ${APT_GET} install -y resolvconf net-tools dnsutils

if false && ! command -v glinux-updater; then
  sudo insert-text-block '# ACcJNLRzsCtNjcCpo74lotyQAEgD122R-dns-server'  \
    /etc/resolvconf/resolv.conf.d/head <<EOF
nameserver 77.88.8.8
nameserver 77.88.8.1
EOF
fi

X_WWW_BROWSER=$(which x-www-browsers) || X_WWW_BROWSER="/usr/bin/x-www-browser"

if which "${X_BROWSER}" && which update-alternatives; then
  sudo update-alternatives --install "${X_WWW_BROWSER}"  \
    x-www-browser ${X_BROWSER} 200
  sudo update-alternatives --set x-www-browser ${X_BROWSER}
  # sudo update-alternatives --config x-www-browser
fi

sudo insert-text-block \
     '# yTyIZrilAW59XgITTlNwLp3VdhMn9k7R-enable-sysrq' \
     /etc/sysctl.d/99-sysctl.conf<<EOF
kernel.sysrq = 1
EOF

sagiy network-manager ufw

sudo ufw enable
sudo ufw allow 22/tcp

sudo insert-text-block \
     '# 28bdb8b6-b290-4826-9210-fc8556998230-ssh-no-password-authentication'  \
     /etc/ssh/sshd_config<<EOF
PasswordAuthentication no
PermitEmptyPasswords no
EOF

sudo insert-text-block \
     '# 2c8739f8-5dfb-4329-8b73-afacddddef11-dont-revert-spoofed-mac'  \
     /etc/NetworkManager/NetworkManager.conf <<EOF || true
[device]
wifi.scan-rand-mac-address=no

[connection]
ethernet.cloned-mac-address=preserve
wifi.cloned-mac-address=preserve
EOF

for SCRIPT in nonet.sh redshift; do
    if ! ./${SCRIPT}; then
        echo "WARNING: failed to install ${SCRIPT}";
    fi
done

sagiy pgformatter

pushd .
cd ~/git/githost
if ! pip install .; then
    echo "WARN: failed to install githost"
fi
popd

sagiy figlet

figlet $(hostname) | sudo tee /etc/motd

echo "success"
