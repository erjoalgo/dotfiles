#!/bin/bash

set -euo pipefail

test 0 -ne "${EUID}"

# get sudo, git and curl
APT_GET=$(which apt-get yum 2>/dev/null) || true
SUDOCMD="su -c"
if which sudo && SUDO_ASKPASS=$(which false) sudo true; then
    SUDOCMD="sudo bash -c"
fi

if test -n "${APT_GET}"; then
   ${SUDOCMD} "${APT_GET} install -y git sudo curl python"
fi

# fetch my git repos
GIT_EMAIL=erjoalgo@gmail.com
GIT_NAME="Ernesto Alfonso"
GIT_HOME=${HOME}/git
mkdir -p ${GIT_HOME}
for REPO in erjoalgo-stumpwmrc \
		dotemacs \
	    ; do
    cd ${GIT_HOME}
    test -d ${REPO} || git clone "https://github.com/erjoalgo/${REPO}"
    cd ${REPO}
    git config user.name "${GIT_NAME}"
    git config user.email ${GIT_EMAIL}
done

# no realpath on macos...
SCRIPTS_BIN=$(pwd)/../scripts/bin
export PATH=$PATH:${SCRIPTS_BIN}

which insert-text-block
sudo ln -fs $(which insert-text-block) /usr/bin

# set up passwordless sudo
NOPASSWD_LINE="${USER} ALL=(ALL:ALL) NOPASSWD:ALL"
if test -d /etc/sudoers.d; then
    # use sudoers.d if possible
    ${SUDOCMD} "$(which insert-text-block) \#\ 9003681e-db65-4817-9bf0-c4329f0d261b-add-${USER}-sudo-nopasswd /etc/sudoers.d/nopasswd" \
               <<< "${NOPASSWD_LINE}"
    # INCLUDE_SUDOERS_LINE="#includedir /etc/sudoers.d"
elif ! ${SUDOCMD} "grep -F \"${LINE}\" /etc/sudoers"; then
    ${SUDOCMD} "tee -a /etc/sudoers <<< \"${NOPASSWD_LINE}\""
fi

# sudo ln -sf  /usr/local/bin
# sudo which insert-text-block

# set default cmd line editor to vi
sudo update-alternatives --set editor /usr/bin/vim.tiny --verbose || true
# set firefox-new-tab.sh as browse command
FIREFOX_NEW_TAB=$(which firefox-new-tab)
sudo update-alternatives --install $(which x-www-browser) \
     x-www-browser ${FIREFOX_NEW_TAB} 200 || true
sudo update-alternatives --set x-www-browser ${FIREFOX_NEW_TAB} || true

# link inits
cd ~/git/erjoalgo-stumpwmrc/scripts/
for SCRIPT in link-inits.sh\
                  gen-git-config.sh \
	      ;do
    ./${SCRIPT}
done

# needs to go after link-inits.sh
insert-text-block '# bbdede6e-87c5-4ba9-927e-78865afb3dcb-source-my-bashrc'  \
		  ${HOME}/.bashrc <<<"source ${HOME}/.my-bashrc"

XSESSIONRC=${HOME}/.xsessionrc
touch ${XSESSIONRC}
# lightdm does not source ~/.profile
# https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=636108
for SHADOWER in ~/.profile ~/.bash_profile ~/.bash_login ${XSESSIONRC}; do
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

which apt-get && sudo apt-get install -y apt-file && sudo apt-file update || true

if which apt-get && grep cdrom /etc/apt/sources.list; then
    sudo ./update-sources-list.sh
fi
sudo apt-get update

# some essential scripts
if test -n "${APT_GET}"; then
    sudo ${APT_GET} install -y htop auditd fail2ban bootlogd unison
fi

insert-text-block '# 91352955-c448-4c16-a4d4-54470089c900-notify-lagging-repos-user-crontab' \
    <(crontab -l 2>/dev/null) -o >(crontab) <<EOF
30 10 * * * bash -c '${SCRIPTS_BIN}/git-notify-lagging-repos.sh ~/git/*'
EOF

insert-text-block '# 99ef88b9-660b-458d-9dfd-9cf090778ea5-include-private-ssh-config' \
                  ~/.ssh/config -b <<EOF
Include ~/private-data/configs/ssh-config
EOF

if ! ssh -G google.com; then
    insert-text-block '# 99ef88b9-660b-458d-9dfd-9cf090778ea5-include-private-ssh-config'
    ~/.ssh/config -b <<EOF
# Include ~/private-data/configs/ssh-config disabled...
EOF
fi

echo "success"
