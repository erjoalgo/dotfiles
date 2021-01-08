function make-completion-wrapper ()  {
    # from https://unix.stackexchange.com/questions/4219/

    # example
    # make-completion-wrapper _apt_get _apt_get_install apt-get install
    # complete -F _apt_get_install apt-inst

    local comp_function_name="$1"
    local function_name="$2"
    local arg_count=$(($#-3))
    shift 2
    local function="
    function $function_name {
      ((COMP_CWORD+=$arg_count))
      COMP_WORDS=( "$@" \${COMP_WORDS[@]:1} )
      "$comp_function_name"
      return 0
    }"
    eval "$function"
}

function complete-alias  {
    # uses make-completion-wrapper: https://unix.stackexchange.com/a/4220/50978
    # example usage
    # complete-alias _pass pshow pass show
    # complete-alias _pass pgen pass generate

    EXISTING_COMPLETION_FN=${1} && shift
    ALIAS=${1} && shift
    AUTOGEN_COMPLETION_FN="__autogen_completion_${ALIAS}"
    make-completion-wrapper ${EXISTING_COMPLETION_FN} ${AUTOGEN_COMPLETION_FN} \
                            ${*}
    COMMAND=${1} # don't shift
    LAZY_LOAD_COMPLETION_FN="__lazy_complete_$ALIAS"
    local function="
    function ${LAZY_LOAD_COMPLETION_FN} {
      _completion_loader ${COMMAND}
      complete -F ${AUTOGEN_COMPLETION_FN} ${ALIAS}
      return 0
    }"
    eval "$function"
    complete -F "${LAZY_LOAD_COMPLETION_FN}" "${ALIAS}"
}


#shortcuts
alias cdrealpath='pwd; cd $(realpath .); pwd'
alias cdpushd='pushd .; cd'
alias j='jobs'
alias e='emacsclient -n'
alias enw='emacs -nw'
alias g='grep'
alias l='less -R'
alias c='cat'
alias gi='grep -i'
alias gol='grep -o'
alias grepc='grep --color=always'
if command -v xsel > /dev/null; then
    alias xs='xsel -ib'
elif command -v pbcoby; then
    alias xs='pbcopy'
else
    alias xs='cat'
fi
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
  source ${GIT_COMPLETIONS}
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
# http://stackoverflow.com/questions/3515597/add-only-non-whitespace-changes
alias ganw='git diff -U0 -w --no-color "$@" | git apply --cached --ignore-whitespace --unidiff-zero -'
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
alias gsh='git stash'
alias grvt='wte git-revert-HEAD'

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
        complete-alias _apt_get ${ALIAS} apt-get install
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



#meta
alias .a='source ~/.bash_aliases && for SRC in $(find -L ~/.bash-fns/ -type f) ~/.profile-env; do source $SRC; done'
alias .brc='source ~/.bashrc'
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
alias lagging-repos='git-notify-lagging-repos.sh ~/git/*'
alias less-auth='sudo less -f /var/log/auth.log'
alias less-syslog='sudo less -f /var/log/syslog'
alias less-mail='sudo less /var/mail/$(whoami)'
alias tail-auth='sudo tail -f /var/log/auth.log'
alias tail-syslog='sudo tail -f /var/log/syslog'
alias tail-mail='sudo tail /var/mail/$(whoami)'
alias mci='mvn clean install'
alias cflogs='cf logs $(grep -Po "(?<=name: ).*" manifest.yml)'
alias json-pp='python -mjson.tool'

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
alias pem-sha256='openssl x509 -noout -fingerprint -sha256 -in'

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
alias echo-pings='sudo tcpdump ip proto \\icmp'
alias gen-cert="openssl req  -nodes -new -x509  -keyout server.key -out server.cert"
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
alias nmap-list-ssl-ciphers='nmap --script ssl-enum-ciphers -p 443'

alias sv=service
alias sv-tail='sudo journalctl -fu'

# https://www.commandlinefu.com/commands/view/5410/intercept-stdoutstderr-of-another-process
alias strace-attach-stdout='strace -ff -e trace=write -e write=1,2 -s99999 -p'

alias luks-add-key="sudo cryptsetup luksAddKey"


# Local Variables:
# mode: sh
# End:
