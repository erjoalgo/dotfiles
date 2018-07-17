#!/bin/bash -x

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

# set up passwordless sudo
LINE="${USER} ALL=(ALL:ALL) NOPASSWD:ALL"
${SUDOCMD} "grep -F \"${LINE}\" /etc/sudoers" || ${SUDOCMD} "tee -a /etc/sudoers <<< \"${LINE}\""

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

GNU_SCRIPTS=${GIT_HOME}/erjoalgo-gnu-scripts
cd ${GNU_SCRIPTS} && git pull --ff-only
export PATH=$PATH:${GNU_SCRIPTS}

ADDBLOCK=${GNU_SCRIPTS}/insert-text-block
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
mkdir -p ${HOME}/.config
cd ~/git/erjoalgo-stumpwmrc/scripts/
for SCRIPT in link-inits.sh\
	      ;do
    ./${SCRIPT}
done

# needs to go after link-inits.sh
insert-text-block '# bbdede6e-87c5-4ba9-927e-78865afb3dcb-source-my-bashrc'  \
		  ${HOME}/.bashrc <<<"source ${HOME}/.my-bashrc"

insert-text-block '# 5a82826a-aad9-11e7-872b-4fada3489c57-source-my-bashrc'  \
		  ${HOME}/.bash_profile <<< "source ${HOME}/.my-bash-profile"

sed -i '/^HIST\(FILE\)\?SIZE=[0-9]*/d' "${HOME}/.bashrc" || true
# set GRUB timeout to zero
GRUB_FILE=/etc/default/grub
if test -e ${GRUB_FILE} &&  \
	grep -P '^GRUB_TIMEOUT=[0-9]+$' ${GRUB_FILE} | grep -v '=0'; then
    sudo sed -i 's/^GRUB_TIMEOUT=[0-9]*$/GRUB_TIMEOUT=0/' ${GRUB_FILE}
    which update-grub && sudo update-grub
fi

# some essential scripts
if test -n "${APT_GET}"; then
    sudo ${APT_GET} install -y htop auditd fail2ban bootlogd
fi

which apt-get && sudo apt-get install -y apt-file && sudo apt-file update || true

if which apt-get && grep cdrom /etc/apt/sources.list; then
    sudo ./update-sources-list.sh
fi

insert-text-block '# 91352955-c448-4c16-a4d4-54470089c900-notify-lagging-repos-user-crontab' \
    <(crontab -l 2>/dev/null) -o >(crontab) <<EOF
30 10 * * * bash -c '~/git/erjoalgo-gnu-scripts/git-notify-lagging-repos.sh ~/git/*'
EOF

mkdir -p ${HOME}/src

echo "success"
