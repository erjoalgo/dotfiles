. ${HOME}/git/dotfiles/submodules/complete-alias/complete_alias

function make-completion-wrapper ()  {
    echo "WARN: calling deprecated function make-completion-wrapper"
    return
}

function complete_alias {
    ALIAS=${1} && shift
    complete -F _complete_alias "${ALIAS}"
}

function complete-alias  {
    echo "WARN: calling deprecated function complete-alias"
    return
}

#shortcuts
alias cdrealpath='pwd; cd $(realpath .); pwd'
alias cdpushd='pushd .; cd'
alias j='jobs'
alias e='emacsclient-wrapper.sh -n'
alias enw='emacs -nw'
alias g='grep'
alias l='less -R'
alias c='cat'
alias gi='grep -i'
alias gol='grep -o'
alias grepc='grep --color=always'
if command -v x-service-curl > /dev/null; then
    alias xs='x-service-curl /clipboard -i'
elif command -v xsel > /dev/null; then
    alias xs='xsel -ib'
elif command -v pbcoby; then
    alias xs='pbcopy'
else
    alias xs='cat'
fi

function xC {
    FILENAME=${1} && shift
    ABS=$(realpath "${FILENAME}" | tr -d '\n' | tr -d '\r')
    echo -n "${ABS}" | xs
    echo "copied: ${ABS}"
}

alias pd='pushd .'
alias ppd='popd'

alias xargsz='xargs -IZ'
alias ps-aux-grep='ps aux|grep -i '
alias nnn='sudo netstat -tulpn | less'
# never used
# alias tail1='tail -1'
# alias head1='head -1'
# alias j='jobs'
alias ssh-nohostcheck="ssh -o 'UserKnownHostsFile /dev/null' -o 'VerifyHostKeyDNS no' -o 'StrictHostKeyChecking no'"
alias nc-android='ifconfig |grep inet && echo "nc -l -p 4711" && nc -l -p 4711 | tar xv'
alias lynx-accept-all-cookies='lynx -accept_all_cookies'


#git
# http://stackoverflow.com/questions/342969/
GIT_COMPLETIONS=/usr/share/bash-completion/completions/git
if test -f ${GIT_COMPLETIONS}; then
  . ${GIT_COMPLETIONS}
else
  function __git_complete {
      true
  }
fi
unset GIT_COMPLETIONS

alias gt='git status'
__git_complete gt _git_status
alias gf='git diff'
alias gfc='git diff --cached'
__git_complete gf _git_diff
__git_complete gfc _git_diff
alias gl='git log'
__git_complete gl _git_log
alias grl='git reflog'
__git_complete grl _git_reflog
alias ga='__git-commit-interactive-wrapper git add'
__git_complete ga _git_add
alias grv='git remote -vv'
# alias gpom='git push origin master'
alias gitresetsoft='git reset --soft HEAD~1'
alias grh='__git-commit-interactive-wrapper git reset HEAD'
alias grhp='__git-commit-interactive-wrapper git reset HEAD -p'
alias grr='git reset --hard'
alias gb='git branch --sort=-committerdate'
__git_complete gb _git_branch
__git_complete grr _git_branch
alias gmbs='git-merge-base-show'
__git_complete gmb _git_diff
#bash completion will do the rest below
alias gmmma='git commit -a -m "autocommit on $(date)"'
alias gmmma-gpom='git add -A . && gmmma && gpom'
alias git-stash-unstaged='git stash push --keep-index'

# http://stackoverflow.com/questions/3515597/add-only-non-whitespace-changes

function ganw {
    git diff -U0 -w --no-color "${@}" |  \
        git apply --cached --ignore-whitespace --unidiff-zero -
}

alias gaow='git add -A; git diff --cached -w | git apply --cached -R'
alias __git-commit-interactive-wrapper='VISUAL="vim" getchwrap eynqsh?da -p "([sS]tage|Discard|Edit again|Apply|Stash).+[?] " --'
alias gcp='__git-commit-interactive-wrapper git commit -p'
alias gcpa='__git-commit-interactive-wrapper git commit -p --amend'
alias gcpan='__git-commit-interactive-wrapper git commit -p --amend --no-edit'
alias gcpn='__git-commit-interactive-wrapper git commit -p --no-edit'
alias gspa='__git-commit-interactive-wrapper git stash -p'
__git_complete gcp _git_commit
__git_complete gcpa _git_commit
__git_complete gcpan _git_commit
alias gpff='git pull --ff-only'
__git_complete gpff _git_pull
alias gmff='git merge --ff-only'
__git_complete gmff _git_merge
alias gfr='git fetch && git rebase'
__git_complete gfr _git_rebase
alias gkt='__git-commit-interactive-wrapper git checkout'
__git_complete gkt _git_checkout
alias gkp='__git-commit-interactive-wrapper git checkout -p'
alias gcrp='__git-commit-interactive-wrapper git cherry-pick'
__git_complete gcrp _git_cherry_pick
alias gkp-last='__git-commit-interactive-wrapper git checkout -p HEAD^'
__git_complete gkp _git_checkout
__git_complete gkp-last _git_checkout
alias gw='git show'
alias gww='git show HEAD^'
alias gwww='git show HEAD^^'
alias gwwww='git show HEAD^^^'
alias gwwwww='git show HEAD^^^^'
alias gwwwwww='git show HEAD^^^^^'
__git_complete gw _git_show
alias cdgittop='cd $(git rev-parse --show-toplevel) && pwd'
alias hb='hub browse'
# stands for git log "one by one"
alias gl11='git-review-pull-request'
__git_complete gl11 _git_log
alias grc='git rebase --continue'
alias grs='git rebase --skip'
alias gsw='git stash show -p'
alias gsd='git stash drop'
alias gsl='git stash list'
alias gsp='git stash pop'
alias gsh='__git-commit-interactive-wrapper git stash'
alias grvt='wte git-revert-HEAD'
alias gcl='git clone'

# never used
# alias gm='git commit'
# alias gmm='git commit -a -m'
# alias grso='git remote show origin'
# alias gra='git remote add'
# alias grr='git remote remove'
# alias grr='git remote rm'
# alias gmmm='git commit -a -m "autocommit on $(date)"'
# alias gamm='git add -A; git commit -a -m'
# alias gitstatus='git status'
# alias gitcommit='git commit'

#apt
#alias sagi='sudo apt-get install'
if command -v apt-get > /dev/null; then
    alias affexact='bash -xc '\''apt-file find $0 | grep "/$0$"'\'''
    alias aff='apt-file find'
    alias afl='apt-file list'
    alias sagiy='sudo apt-get install -y'
    for ALIAS in sagiy acs acw dpkgl; do
        complete_alias ${ALIAS} apt-get
    done

    alias sagu='sudo apt-get update'
    alias acw='apt-cache show'
    alias acs='apt-cache search'
    alias dpkgl='dpkg -L'
else
    alias sagiy='sudo yum install -y'
    alias aff='yum provides'
    alias acw='yum info'
    alias acs='yum search'
    alias affexact='bash -xc '\''yum provides "*/$0"'\'''
    complete -F _command affexact
    alias dpkgl='repoquery -l'
fi

alias spsi='sudo python setup.py install'


function source-personal-bash-files {
  for SRC in $(find -L ~/.bash-fns/ -type f)  \
                 ~/.profile-env \
                 ~/.bash_aliases \
                 ~/.my-bashrc \
             ; do
    echo sourcing $SRC
    . $SRC
  done
}

alias .a=source-personal-bash-files
alias .brc='. ~/.bashrc'
alias .t='tmux source ~/.tmux.conf'
alias .r='tput reset; clear'
alias aa='aliasadd.py'

#misc programs
alias untar='tar axvf'
alias thon='python'
alias ura='zathura'
alias mmln='move-last-n.sh'

# useful arguments as new commands
alias duh='du -h'
alias ffdefault='firefox -P default'
alias lp-one-sided='lp -o sides=one-sided'
alias untar-src='tar -C ~/src/ -axvf'
alias unzip-src='unzip -d ~/src'
alias chmodx='chmod +x'
alias chown-rec='sudo chown -R '
complete -F _usermod chown-rec
alias gitlag='git-notify-lagging-repos.sh ~/git/*'
alias less-auth='sudo less -f /var/log/auth.log'
alias less-syslog='sudo less -f /var/log/syslog'
function grep-syslog {
    sudo grep -a ${*} /var/log/syslog
}
alias less-mail='sudo less /var/mail/$(whoami)'
alias tail-auth='sudo tail -f /var/log/auth.log'
alias tail-syslog='sudo tail -f /var/log/syslog'
alias tail-mail='sudo tail -f /var/mail/$(whoami)'
alias less-mail='sudo less /var/mail/$(whoami)'
alias mci='mvn clean install'
alias cflogs='cf logs $(grep -Po "(?<=name: ).*" manifest.yml)'
alias json-pp='python3 -mjson.tool'

#pipes
alias lein-repl-tee-log='lein repl |& tee log'
alias psgrep='ps ax | grep -i'
alias echolastcmd="fc -ln -1 | xsel --clipboard"
alias javafmt-pr='git filter-branch -f --tree-filter "javafmt ." origin/master..HEAD'
alias cat-id-rsa-pub-xs='cat ~/.ssh/id_rsa.pub | tee /dev/stderr | xs'
alias ssh-ecdsa-fingerprint='ssh-keygen -lf /etc/ssh/ssh_host_ecdsa_key.pub'
alias ssh-ecdsa-fingerprint-md5='ssh-ecdsa-fingerprint -Emd5'
alias ssh-ecdsa-fingerprint-sha256='ssh-ecdsa-fingerprint -Esha256'
alias ssh-ecdsa-fingerprint-sha256-old-ssh="awk '{print \$2}' /etc/ssh/ssh_host_ecdsa_key.pub | base64 -d | sha256sum -b | awk '{print \$1}' | xxd -r -p | base64"

function cert-https-server-fingerprint  {
    SERVER=${1} && shift
    openssl s_client -connect ${SERVER} < /dev/null 2>/dev/null |  \
        openssl x509 -fingerprint -noout -in /dev/stdin
}

function cert-https-server-print  {
    SERVER=${1} && shift
    FILENAME=${1} && shift
    echo -n | openssl s_client -connect ${SERVER} \
        | openssl x509
}


function cert-list-ssh-host-key-fingerprints {
    for FILE in /etc/ssh/*pub; do
        ssh-keygen -lf ${FILE} -E sha256;
    done
}

# command defaults
if ls --color=auto &> /dev/null; then
    alias ls='ls --color=auto'
fi
alias pkill='pkill -e'
alias sbcl='rlwrap sbcl'
alias gdmapf='gdmap -f'
alias pip-install-user='pip install --user'

# auto sudo
alias service='sudo service'
alias umount='sudo umount'
# complete -F _umount_module umount
complete -F _command umount
alias blkid='sudo blkid'
alias killall='sudo killall -v'

# hard-to-remember or reproduce
alias grep-ip="sudo grep -o '\([0-9]\{1,3\}[.]\)\{3\}[0-9]\{1,3\}'"
alias fail2ban-unban-ip='sudo fail2ban-client set ssh unbanip'
alias ping-echo='sudo tcpdump ip proto \\icmp'
alias gen-cert="openssl req  -nodes -new -x509  -keyout server.key -out server.cert"
alias cert-cat='sudo openssl x509 -noout -text -in'

alias httpd="python -m SimpleHTTPServer"

alias gpg-list-keys-long-format='gpg --list-keys --keyid-format long'
#
alias nvm-install-stable='nvm install stable && nvm alias default node'

alias ..2='cd ../..'
alias ..3='cd ../../..'
alias ..4='cd ../../../..'
alias ..5='cd ../../../../..'
# alias ..6='cd ../../../../../..'

alias with-vi-editor='EDITOR=vim VISUAL=vim'
complete -F _command with-vi-editor
alias wve='with-vi-editor'
complete -F _command wve

alias wte='EDITOR=true VISUAL=true'
complete -F _command wte

alias xinitrc-reload='sbcl --script ~/.stumpwmrc.d/lisp/xinitrc.lisp'
alias XDOTOOL-PRESS-CAPSLOCK='xdotool key Caps_Lock'
alias mic-check="arecord -vv -f dat /dev/null"
alias tma='tmux-attach'
alias dd-status='watch -n 20 sudo kill -USR1 $(pidof dd)'

alias bash-login-debugging-list-open-files="echo exit | strace bash -li |& less | grep ^open"

alias git-delete-show-untracked-files='git ls-files --others --exclude-standard'
alias git-delete-show-ignored-files='git clean -ndX'
alias git-delete-show-dangling-commits='git fsck --no-reflogs | sed "s/^dangling \(commit\|blob\) //g" | xargs git log --no-walk'
alias git-delete-show-unpublished-commits='git --no-pager log --branches --not --remotes'
alias git-undo='git reset --soft HEAD@{1}'
alias nmap-list-ssl-ciphers='nmap --script ssl-enum-ciphers -p 443'

alias sv=service

alias sudo='sudo '

complete_alias sudo

for LETTER_COMMAND in "down stop" "r restart"  \
                               "s status" "l logs"  \
                               "up start"; do
    COMMAND=$(cut -f2  -d' '  <<< "${LETTER_COMMAND}")
    LETTER=$(cut -f1  -d' '  <<< "${LETTER_COMMAND}")
    for USR_OPT in "" "u"; do
        ALIAS="s${USR_OPT}${LETTER}"
        if test "${COMMAND}" = logs; then
            if test -z "${USR_OPT:-}"; then
                alias ${ALIAS}="sudo journalctl -feu"
            else
                alias ${ALIAS}="journalctl --user -feu"
            fi
        else
            if test -z "${USR_OPT:-}"; then
                alias ${ALIAS}="sudo systemctl ${COMMAND}"
            else
                alias ${ALIAS}="systemctl --user ${COMMAND}"
            fi
        fi
        complete_alias ${ALIAS}
    done
done

function __svlogs-multi {
    set -x
    LAST=()
    for SERVICE_OR_FILE in ${*}; do
        if test -e ${SERVICE_OR_FILE}; then
            sudo tail -f "${SERVICE_OR_FILE}"
            LAST+=($!)
        else
            sudo journalctl -fu ${SERVICE_OR_FILE} &
            LAST+=($!)
        fi
    done
    for PID in "${LAST[@]}"; do
        echo "DDEBUG rs3p .bash_aliases VALUE OF \PID: $PID"
        wait ${PID}
    done
    set +x
}

function svlogs-multi {
    (trap 'kill 0' SIGINT; __svlogs-multi ${*})
}

# TODO support repeated completion of multiple service arguments

complete_alias svlogs-multi

# complete -F _service service-disable-stop-remove stop
alias service-delete='service-disable-stop-remove'
complete_alias service-delete

# https://www.commandlinefu.com/commands/view/5410/intercept-stdoutstderr-of-another-process
alias strace-attach-stdout='sudo strace -ff -e trace=write -e write=1,2 -s99999 -p'

alias luks-add-key="sudo cryptsetup luksAddKey"
alias lsdisk="sudo lsblk -o NAME,FSTYPE,SIZE,MOUNTPOINT"
alias ifconfig="ip address show"

alias sudo='sudo ' # allow combining sudo with aliases
alias groups-newline="groups | tr ' ' '\n' | less"
alias ndw='nmcli d w'
alias ndwc='nmcli d w c'

alias g4r='g4 revert'
alias g4df='g4 revert'

alias modernize-directory="find . -name '*py' -exec modernize -wn {} +"
alias gpg-clearsign-test='gpg --clearsign <<< test'
function android-pull-rm-media {
    android-find-pull-rm.sh sdcard/DCIM -name "'*mp4'" -o -name "'*jpg'"
}

function docker-compose-exec {
    docker compose exec $(docker-select-container) ${*}
}
alias dcl='docker compose logs -f --tail 1000'
alias dcps='docker compose ps'
alias dcu='docker compose up -d; docker compose logs -f'
function dcub {
    docker compose up --build -d ${*}
    docker compose logs -f ${*}
}

alias dcr='docker compose restart;'
alias dc='docker compose'
alias db='docker-util bash'
alias dpush='docker-util push'
alias dpull='docker-util pull'
alias dps='docker ps'
alias dc-exec='docker-compose-exec'

alias lrsh='lein ring server-headless'

alias opp='octoprint-print.sh -g'
alias opu='octoprint-cli files upload'

alias redshift-restart='systemctl --user restart redshift.service'
alias redshift-stop='systemctl --user stop redshift.service'
alias redshift-logs='journalctl --user -fu redshift'

alias tor-browser='~/src/tor-browser/Browser/start-tor-browser'

function ufw-select-rule {
    echo "select ufw rule: " 1>&2
    sudo ufw status numbered | head -3 1>&2
    OLDIFS=$IFS
    IFS=$'\n'
    select RULE in $(sudo ufw status numbered | grep ']'); do
        break
    done
    IFS=$OLDIFS
    grep -Po '(?<=[[]) *[0-9]+(?=])' <<< "${RULE}" | tr -d ' '
}

function ufw-allow-tcp {
    PORTS=${1} && shift
    sudo ufw allow ${PORTS}/tcp
}

alias ufw-ls='sudo ufw status numbered'


alias iptables-list='sudo iptables -vnL --line-numbers | less'
alias ssh-universal='ledger-agent -v erjoalgo@gmail.com ssh'
alias ssh-universal-shell='ledger-agent -vs erjoalgo@gmail.com'
function ssh-universal-copy-id {
    REMOTE_HOST=${1} && shift
    ID_RSA_PUB=$(cat ~/.ssh/id_rsa.pub)
    set -x
    ledger-agent -v erjoalgo@gmail.com  \
                 ssh ${REMOTE_HOST} "tee -a .ssh/authorized_keys <<< '${ID_RSA_PUB}'"
    set +x
}

alias docker-pull='docker-util pull'
alias docker-push='docker-util push'

alias udevadm-monitor='sudo udevadm monitor'
function device-monitor {
    { sudo udevadm monitor &  \
      sudo tail /var/log/syslog | grep -v UFW &  \
      sudo dmesg -w | grep -v UFW; }
}

function cdafs {
    cd "/afs/$(tr -d '\n' < /etc/openafs/ThisCell)/public/"
}
alias efm=ecryptfs-mount.sh

alias gpg-symmetric-encrypt="gpg --symmetric --no-symkey-cache --batch"
alias gpg-symmetric-decrypt="gpg --decrypt --no-symkey-cache --batch"

function cdmkdir {
    DIR=${1} && shift
    mkdir "${DIR}"
    cd "${DIR}"
}

alias mvhere='mv -t.'

alias .idf='deactivate; . $HOME/git/esp-idf/export.sh'

# Local Variables:
# mode: sh
# End:
