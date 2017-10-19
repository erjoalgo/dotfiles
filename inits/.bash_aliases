#shortcuts
alias cdrealpath='pwd; cd $(realpath .); pwd'
alias cdpushd='pushd .; cd'
alias j='jobs'
# alias e='emacs'
alias e='emacsclient -n'
alias ec='emacsclient -n'
alias enw='emacs -nw'
alias g='grep'
alias l='less -R'
alias c='cat'
alias gi='grep -i'
alias gol='grep -o'
if command -v xsel > /dev/null; then
    alias xs='xsel -ib'
else
    alias xs='pbcopy'
fi
alias duh='du -h'
alias xargsz='xargs -IZ'
alias ps-aux-grep='ps aux|grep -i '
alias netstat-tulpn-grep='sudo netstat -tulpn | grep -i'
# never used
# alias tail1='tail -1'
# alias head1='head -1'
# alias j='jobs'
alias ssh-nohostcheck="ssh -o 'UserKnownHostsFile /dev/null' -o 'VerifyHostKeyDNS no' -o 'StrictHostKeyChecking no'"
alias netcatdroid='ifconfig |grep inet && echo "nc -l -p 4711" && nc -l -p 4711 | tar xv'
alias lynx-accept-all-cookies='lynx -accept_all_cookies'


#git
# http://stackoverflow.com/questions/342969/
GIT_COMPLETIONS=/usr/share/bash-completion/completions/git
test -f ${GIT_COMPLETIONS} && source ${GIT_COMPLETIONS}
unset GIT_COMPLETIONS

alias gt='git status'
__git_complete gt _git_status
alias gf='git diff'
alias gfc='git diff --cached'
__git_complete gf _git_diff
__git_complete gfc _git_diff
alias gl='git log'
__git_complete gl _git_log
alias ga='git add'
__git_complete ga _git_add
alias grv='git remote -vv'
# alias gpom='git push origin master'
alias gitresetsoft='git reset --soft HEAD~1'
alias grh='git reset HEAD'
alias gb='git branch'
#bash completion will do the rest below
alias gmmma='git commit -a -m "autocommit on $(date)"'
# http://stackoverflow.com/questions/3515597/add-only-non-whitespace-changes
alias ganw='git diff -U0 -w --no-color "$@" | git apply --cached --ignore-whitespace --unidiff-zero -'
alias gaow='git add -A; git diff --cached -w | git apply --cached -R'
alias gcp='VISUAL=vi git commit -p'
__git_complete gcp _git_commit
alias gpff='git pull --ff-only'
__git_complete gpff _git_pull
alias gfr='git fetch && git rebase'
__git_complete gfr _git_rebase
alias gkt='git checkout'
__git_complete gkt _git_checkout
alias gkp='git checkout -p'
__git_complete gck _git_checkout
alias gw='git show'
__git_complete gw _git_show
alias cdgittop='cd $(git rev-parse --show-toplevel) && pwd'
alias hb='hub browse'
# stands for git log "one by one"
alias gl11='git-review-pull-request'
alias grc='git rebase --continue'
__git_complete gl11 _git_log
alias gss='git stash show -p'
alias gsd='git stash drop'
alias gsl='git stash list'
alias gsp='git stash pop'

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
    _sagiy()
    {

	local cur
	_init_completion || return

	COMPREPLY=( $( apt-cache --no-generate pkgnames "$cur" \
				 2> /dev/null ) )
	return 0
    } &&
	complete -F _sagiy sagiy
    complete -F _sagiy acs
    complete -F _sagiy acw
    complete -F _sagiy dpkgl

    alias sagu='sudo apt-get update'
    alias acw='apt-cache show'
    alias acs='apt-cache search'
else
    alias sagiy='sudo yum install -y'
    alias aff='yum provides'
    alias acw='yum info'
    alias acs='yum search'
    alias affexact='bash -xc '\''yum provides "*/$0"'\'''
    complete -F _command affexact
fi
    
alias spsi='sudo python setup.py install'
alias dpkgl='dpkg -L'


#wifi
alias was='sudo wifi scan'
alias wad='sudo wifi add'
alias wac='sudo pkill -e wpa_supplicant; sudo pkill -e dhclient; sudo wifi -y -t ac'
alias wacc='sudo wifi -y connect'

#meta
alias .a='source ~/.bash_aliases && source ~/.my-bash-funs'
alias .brc='source ~/.bashrc'
alias aa='aliasadd.py'

#misc programs
alias untar='tar axvf'
alias sbcl='rlwrap sbcl'
alias thon='python'
alias ura='zathura'
alias ffdefault='firefox -P default'
alias mmln='move-last-n.sh'
alias gdmapf='gdmap -f'
alias lp-one-sided='lp -o sides=one-sided'
alias passgen='pass generate -n'
alias pip-install-user='pip install --user'
alias lein-repl-tee-log='lein repl |& tee log'

#combinations
alias cataliasgrep='cat ~/.bash_aliases | grep '
alias psgrep='ps ax | grep -i'
alias untarprogram='tar -C ~/programs/ -axvf'
alias unzipprogram='unzip -d ~/programs/'
alias echolastcmd="fc -ln -1 | xsel --clipboard"
alias chmodx='chmod +x'
alias chown-rec='sudo chown -R '
complete -F _usermod chown-rec
alias lagging-repos='git-notify-lagging-repos.sh ~/git/*'

# defaults
alias grep='grep --color=auto'
alias ls='ls --color=auto'
alias pkill='pkill -e'
alias service='sudo service'
alias umount='sudo umount'
alias blkid='sudo blkid'
alias killall='sudo killall -v'

# misc
alias grep-ip="sudo grep -o '\([0-9]\{1,3\}[.]\)\{3\}[0-9]\{1,3\}'"
alias fail2ban-unban-ip='sudo fail2ban-client set ssh unbanip'
# Local Variables:
# mode: sh
# End:
