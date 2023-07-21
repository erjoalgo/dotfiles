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
if which sudo && SUDO_ASKPASS=$(which false) sudo true; then
  SUDOCMD="sudo bash -c"
else
  SUDOCMD="su -c"
fi

if ! which sudo && test -n "${APT_GET}"; then
   ${SUDOCMD} "${APT_GET} install -y sudo ntp" || true
fi

if which apt-get; then
    if true || ! ${SUDOCMD} "apt-get update" ||  \
        ${SUDOCMD} "grep ^deb\ cdrom /etc/apt/sources.list"; then
        DOTFILES_GITHUB_URL=https://raw.githubusercontent.com/erjoalgo/dotfiles/master/
        URL=${DOTFILES_GITHUB_URL}/scripts/installs/update-sources-list.sh
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
        ${SUDOCMD} "apt-get install -y sudo ntp"
    fi
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

sudo apt-get update

sudo apt-get install -y git curl python3 python3-full

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
    git pull --ff-only
  done
  popd
}

fetch-repos dotfiles

cd ~/git/dotfiles/scripts/

SCRIPTS_BIN="${HOME}/git/dotfiles/scripts/bin"
test -d "${SCRIPTS_BIN}"
export PATH=$PATH:${SCRIPTS_BIN}

which insert-text-block
sudo ln -fs ${SCRIPTS_BIN}/insert-text-block /usr/bin

sudo insert-text-block \
  '# ac25d55a-723b-4da2-bfa6-674c51e7eefb-max-out-virtual-terminal-size' \
  /etc/default/console-setup<<EOF
FONTFACE="TerminusBold"
FONTSIZE="16x32"
EOF

sudo sed -i 's/^XKBOPTIONS=/# \0/g' /etc/default/keyboard

sudo insert-text-block  \
  '# 81475907-8f33-420a-b002-118c8d4a62ae-configure-console-keyboard'  \
  /etc/default/keyboard <<EOF
XKBOPTIONS="terminate:ctrl_alt_bksp,ctrl:nocaps"
EOF

sudo service console-setup restart

if test "${BOOT:-}" = wifi; then
    ./installs/wifi-boot.sh
    echo "completed wifi install"
    exit 0
fi

if test 0 -eq "${EUID}"; then
  echo "do not run this script as root"
fi

sudo ${APT_GET} install -y python3-pip vim

if false; then
    python3 -m venv ~/.venv
    source ~/.venv/bin/activate
fi

pip3 install getchwrap -U

# link inits
for SCRIPT in  \
    ./installs/link-inits.sh \
        ./installs/gen-git-config.sh \
    ;do
    ./${SCRIPT}
done

# set default cmd line editor to vi
sudo update-alternatives --set editor /usr/bin/vim --verbose || true

EMACSCLIENT_WRAPPER=${HOME}/.stumpwmrc.d/scripts/bin/emacsclient-wrapper.sh
sudo update-alternatives --install /usr/bin/emacsclient emacsclient \
  "${EMACSCLIENT_WRAPPER}" 0
sudo update-alternatives --set emacsclient "${EMACSCLIENT_WRAPPER}" --verbose

sudo insert-text-block  \
     '"xUxm084v1yfXHaLwwMFubYIub1eOsvKo-system-wide-vi-settings"'  \
     /usr/share/vim/vimrc \
    < ${HOME}/.vimrc

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
source ${HOME}/.my-profile
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
       apt-file unattended-upgrades ntp nmap bash-completion man-db rsyslog
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

SSH_PRIVATE_KEY_PATH=~/.ssh/id_rsa
if ! test -e ${SSH_PRIVATE_KEY_PATH}; then
    ssh-keygen -t rsa -b 4096 -o -a 100 -N '' -f ${SSH_PRIVATE_KEY_PATH}
fi

touch ~/.ssh/authorized_keys
chmod 600 ~/.ssh/authorized_keys

insert-text-block -b '# -*- mode: sh -*-'  \
                  ~/.profile-env  \
  <<EOF
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
export PATH=\$PATH:${HOME}/git/dotemacs/lisp/scripts
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

insert-text-block "# 391f301e-c328-43f2-84ff-94de868293c7-ssh-send-env-desktop-group-number"  \
  "${HOME}/.ssh/config" << EOF
Match host *
    SendEnv DESKTOP_GROUP_NUMBER
    RemoteForward /tmp/.x-service-$(hostname).sock localhost:1959
EOF

sudo ${APT_GET} install -y openssh-server

SSHD_CONFIG=/etc/ssh/sshd_config
if test -f "${SSHD_CONFIG}"; then
  sudo insert-text-block  \
    '# 59b01e7b-7982-4fc0-8b3d-6a7b7cb43ca0-ssh-accept-env-desktop-group-number' \
    "${SSHD_CONFIG}"  \
    <<EOF
AcceptEnv DESKTOP_GROUP_NUMBER
AcceptEnv DEBUG_INIT
# https://unix.stackexchange.com/questions/427189/
StreamLocalBindUnlink yes
EOF
fi

sudo ${APT_GET} install -y resolvconf net-tools

if ! command -v glinux-updater; then
  sudo insert-text-block '# ACcJNLRzsCtNjcCpo74lotyQAEgD122R-dns-server'  \
    /etc/resolvconf/resolv.conf.d/head <<EOF
nameserver 209.182.235.223
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

for SCRIPT in nonet.sh redshift; do
    if ! ./installs/${SCRIPT}; then
        echo "WARNING: failed to install installs/${SCRIPT}";
    fi
done

cd ~/git/githost
python3 setup.py install --user || true

sudo apt-get install figlet

figlet $(hostname) | sudo tee /etc/motd

echo "success"
