#!/bin/bash -x

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
SUDOCMD="su -c"
if which sudo && SUDO_ASKPASS=$(which false) sudo true; then
    SUDOCMD="sudo bash -c"
fi

if test -n "${APT_GET}"; then
   ${SUDOCMD} "${APT_GET} install -y git sudo curl"
fi

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

if which apt-get; then
    sudo apt-get install -y apt-file unattended-upgrades
    sudo apt-file update
    if sudo grep ^deb\ cdrom /etc/apt/sources.list; then
        sudo ./update-sources-list.sh
        sudo apt-get update
    fi
fi

# fetch my git repos
GIT_EMAIL=erjoalgo@gmail.com
GIT_NAME="Ernesto Alfonso"
GIT_HOME=${HOME}/git
mkdir -p ${GIT_HOME}

for REPO in  \
    erjoalgo-stumpwmrc \
	dotemacs \
        githost \
    ; do
    cd ${GIT_HOME}
    test -d ${REPO} || git clone "https://github.com/erjoalgo/${REPO}"
    cd ${REPO}
    git config user.name "${GIT_NAME}"
    git config user.email ${GIT_EMAIL}
done

SCRIPTS_BIN="${HOME}/git/erjoalgo-stumpwmrc/scripts/bin"
test -d "${SCRIPTS_BIN}"
export PATH=$PATH:${SCRIPTS_BIN}

which insert-text-block
sudo ln -fs ${SCRIPTS_BIN}/insert-text-block /usr/bin

if test -n "${LOWBANDWITH:-}"; then
    # this is on a laptop
    sudo ${APT_GET} install -y wireless-tools links
    if lspci -nn | grep -i "Intel.*Wireless"; then
        sudo ${APT_GET} install firmware-iwlwifi
    else
        echo "unknown wireless card"
        exit 1
    fi
    wifi-connect
    links google.com
    curl google.com
    echo "completed low-bandwith install"
    exit 0
fi

test 0 -ne "${EUID}"

sudo ${APT_GET} install -y python python-setuptools python-pip vim
pip install getchwrap --user

# set default cmd line editor to vi
sudo update-alternatives --set editor /usr/bin/vim --verbose || true

# link inits
cd ~/git/erjoalgo-stumpwmrc/scripts/
for SCRIPT in  \
    link-inits.sh\
        gen-git-config.sh \
    ;do
    ./${SCRIPT}
done

insert-text-block '# bbdede6e-87c5-4ba9-927e-78865afb3dcb-source-my-bashrc'  \
		  ${HOME}/.bashrc <<<"source ${HOME}/.my-bashrc"

insert-text-block '# jPC5VOJsRIpcLjXh9o0mJMgPknbejdjl-source-my-bash-profile'  \
                  "${HOME}/.bash_profile" <<< "source ${HOME}/.my-bash-profile"

XSESSIONRC=${HOME}/.xsessionrc
touch ${XSESSIONRC}
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
		          -b ${SHADOWER} <<< "source ${HOME}/.my-profile"
    fi
done

sed -i '/^HIST\(FILE\)\?SIZE=[0-9]*/d' "${HOME}/.bashrc" || true
# set GRUB timeout to zero
GRUB_FILE=/etc/default/grub
if test -e ${GRUB_FILE} &&  \
	grep -P '^GRUB_TIMEOUT=[0-9]+$' ${GRUB_FILE} | grep -v '=0'; then
    sudo sed -i 's/^GRUB_TIMEOUT=[0-9]*$/GRUB_TIMEOUT=0/' ${GRUB_FILE}
    which update-grub && sudo update-grub
fi

mkdir -p ${HOME}/src

# some essential tools
if test -n "${APT_GET}"; then
    sudo ${APT_GET} install -y htop fail2ban unison tmux wget
    sudo ${APT_GET} install -y bootlogd || true
    # auditd may fail with "audit support not in kernel"
    if ! sudo ${APT_GET} install -y auditd; then
	sudo ${APT_GET} purge -y auditd
    fi
fi

insert-text-block '# 91352955-c448-4c16-a4d4-54470089c900-notify-lagging-repos-user-crontab' \
    <(crontab -l 2>/dev/null) -o >(crontab) <<EOF
30 10 * * * bash -c '${SCRIPTS_BIN}/git-notify-lagging-repos.sh ~/git/*'
EOF

mkdir -p ${HOME}/.ssh
insert-text-block '# 99ef88b9-660b-458d-9dfd-9cf090778ea5-include-private-ssh-config' \
                  ~/.ssh/config -b <<EOF
Include ~/private-data/configs/ssh-config
EOF

if ! ssh -G google.com; then
    insert-text-block '# 99ef88b9-660b-458d-9dfd-9cf090778ea5-include-private-ssh-config' \
                      ~/.ssh/config -b <<EOF
# Include ~/private-data/configs/ssh-config disabled...
EOF
fi

chmod 600 ~/.ssh/config

if ! test -e ~/.ssh/id_rsa.pub; then
    ssh-keygen -t rsa -b 4096 -o -a 100
fi

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

insert-text-block ';; 5ef52c11-e976-4eb5-90fa-38795231059d-load-my-sbclrc' \
   ${HOME}/.sbclrc <<EOF
  ;; #-dbg
  (let ((my-sbclrc (merge-pathnames "git/erjoalgo-stumpwmrc/inits/.my-sbclrc"
                                         (user-homedir-pathname))))
    (when (probe-file my-sbclrc)
      (load my-sbclrc)))
EOF

X_BROWSER=$(which x-www-browser-stumpwm)


sudo ${APT_GET} install -y resolvconf
sudo insert-text-block '# ACcJNLRzsCtNjcCpo74lotyQAEgD122R-dns-server'  \
                  /etc/resolvconf/resolv.conf.d/head <<EOF
nameserver 209.182.235.223
nameserver 209.182.235.223
EOF

X_WWW_BROWSER=$(which x-www-browsers) || X_WWW_BROWSER="/usr/bin/x-www-browser"

if which "${X_BROWSER}" && which update-alternatives; then
  sudo update-alternatives --install "${X_WWW_BROWSER}"  \
    x-www-browser ${X_BROWSER} 200
  sudo update-alternatives --set x-www-browser ${X_BROWSER}
  # sudo update-alternatives --config x-www-browser
fi

echo "success"
