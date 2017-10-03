#!/bin/bash -x

set -euo pipefail

test 0 -ne "${EUID}"

# get sudo, git and curl
APT_GET=$(which apt-get yum 2>/dev/null) || true
SUDOCMD="su -c"
if which sudo && SUDO_ASKPASS=$(which false) sudo true; then
    SUDOCMD="sudo"
fi

${SUDOCMD} ${APT_GET} install -y git sudo curl

# set up passwordless sudo
LINE="${USER} ALL=(ALL:ALL) NOPASSWD:ALL"
grep -F "${LINE}" /etc/sudoers || ${SUDOCMD} tee -a /etc/sudoers <<< "${LINE}"

# fetch my git repos
GIT_HOME=${HOME}/git
mkdir -p ${GIT_HOME}
cd ${GIT_HOME}
for REPO in erjoalgo-stumpwmrc \
		erjoalgo-gnu-scripts \
		dotemacs \
		erjoalgo-firefox-addons \
		erjoalgo-vimfx-config \
	    ; do
    test -d ${REPO} || git clone "https://github.com/erjoalgo/${REPO}"
done

export PATH=$PATH:${GIT_HOME}/erjoalgo-gnu-scripts

ADDBLOCK=${GIT_HOME}/erjoalgo-gnu-scripts/insert-text-block
# sudo ln -sf  /usr/local/bin
# sudo which insert-text-block

# set default cmd line editor to vi
sudo update-alternatives --set editor /usr/bin/vim.tiny --verbose || true
# set firefox-new-tab.sh as browse command
FIREFOX_NEW_TAB=$(which firefox-new-tab)
sudo update-alternatives --install $(which x-www-browser) \
     x-www-browser ${FIREFOX_NEW_TAB} 200 || true
sudo update-alternatives --set x-www-browser ${FIREFOX_NEW_TAB} || true

git config --global user.email "erjoalgo@gmail.com"
git config --global user.name "Ernesto Alfonso"


insert-text-block '# bbdede6e-87c5-4ba9-927e-78865afb3dcb-source-my-bashrc'  \
		  ${HOME}/.bashrc <<<"source ${HOME}/.my-bashrc"
sed -i '/^HIST\(FILE\)\?SIZE=[0-9]*/d' "${HOME}/.bashrc"
# set GRUB timeout to zero
GRUB_FILE=/etc/default/grub
if test -e ${GRUB_FILE} &&  \
	grep -P '^GRUB_TIMEOUT=[0-9]+$' ${GRUB_FILE} | grep -v '=0'; then
    sudo sed -i 's/^GRUB_TIMEOUT=[0-9]*$/GRUB_TIMEOUT=0/' ${GRUB_FILE}
    which update-grub && sudo update-grub
fi

# link inits, install stumpwm
cd ~/git/erjoalgo-stumpwmrc/scripts/
for SCRIPT in link-inits.sh\
	      ;do
    # install-stumpwm.sh
    ./${SCRIPT}
done

# some essential scripts
sudo ${APT_GET} install -y htop auditd fail2ban bootlogd
which apt-get && sudo apt-get install -y apt-file && sudo apt-file update || true
