#shortcuts
alias cdrealpath='pwd; cd $(realpath .); pwd'
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
alias xs='xsel -ib'
alias duh='du -h'
alias xargsz='xargs -IZ'
# never used
# alias tail1='tail -1'
# alias head1='head -1'
# alias j='jobs'


#git
# http://stackoverflow.com/questions/342969/
source /usr/share/bash-completion/completions/git
alias gt='git status'
__git_complete gt _git_status
alias gf='git diff'
__git_complete gf _git_diff
alias gl='git log'
__git_complete gl _git_log
alias ga='git add'
__git_complete ga _git_add
alias grv='git remote -vv'
alias gpom='git push origin master'
alias gitresetsoft='git reset --soft HEAD~1'
alias gitresethead='git reset HEAD'
alias gb='git branch'
#bash completion will do the rest below
alias gmmma='git commit -a -m "autocommit on $(date)"'
# http://stackoverflow.com/questions/3515597/add-only-non-whitespace-changes
alias ganw='git diff -U0 -w --no-color "$@" | git apply --cached --ignore-whitespace --unidiff-zero -'
alias gcp='VISUAL=vi git commit -p'
alias gpff='git pull --ff-only'
__git_complete gpff _git_pull
alias gfr='git fetch && git rebase origin/master'
alias gkt='git checkout'
__git_complete gck _git_checkout
alias gkp='git checkout -p'
__git_complete gck _git_checkout
alias gw='git show'
__git_complete gw _git_show
alias cdgittop='cd $(git rev-parse --show-toplevel) && pwd'
alias hb='hub browse'
# stands for git log "one by one"
alias gl11='git-review-pull-request'
__git_complete gl11 _git_log

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
alias sagiy='sudo apt-get install -y'
alias sagu='sudo apt-get update'
alias aff='apt-file find'
alias acs='apt-cache show'
alias spsi='sudo python setup.py install'
alias affexact='bash -xc '\''apt-file find $0 | grep "/$0$"'\'''
alias dpkgl='dpkg -L'


#wifi
alias was='sudo wifi scan'
alias wad='sudo wifi add'
alias wac='sudo wifi -y -t ac'
alias wacc='sudo wifi -y connect'

#meta
alias .a='source ~/.bash_aliases'
alias .brc='source ~/.bashrc'
alias aa='aliasadd.py'

#service. never used
alias service='sudo service'

#misc programs
alias untar='tar axvf'
alias umount='sudo umount'
alias sbcl='rlwrap sbcl'
alias blkid='sudo blkid'
alias thon='python'
alias ura='zathura'
alias ffdefault='firefox -P default'
alias mmln='move_last_n.py'
alias gdmapf='gdmap -f'
alias lp-one-sided='lp -o sides=one-sided'

#bash defaults
alias ls='ls --color=auto'
alias grep='grep --color=auto'

#combinations
alias cataliasgrep='cat ~/.bash_aliases | grep '
alias psgrep='ps ax | grep -i'
alias untarprogram='tar -C ~/programs/ -axvf'
alias unzipprogram='unzip -d ~/programs/'
alias echolastcmd="fc -ln -1 | xsel --clipboard"
alias chmodx='chmod +x'


alias sp='source ~/proxy.sh'

# Local Variables:
# mode: sh
# End:
