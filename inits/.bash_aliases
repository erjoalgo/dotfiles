#shortcuts
alias e='emacs'
alias enw='emacs -nw'
alias ec='emacsclient -n'
alias g='grep'
alias l='less -R'
alias c='cat'
alias gi='grep -i'
alias gol='grep -o'
alias xs='xsel -ib'
alias duh='du -h'
# never used
# alias tail1='tail -1'
# alias head1='head -1'
# alias j='jobs'


#git
alias gt='git status'
alias gf='git diff'
alias gl='git log'
alias ga='git add'
alias gsin='git-commit-select-files.sh'
alias grv='git remote -vv'
alias gpom='git push origin master'
alias gpgm='git push github-origin master'
alias gituncommit='git reset --soft HEAD~1'
alias gitcheckout='git checkout'
alias gb='git branch'
alias gitpulloriginmaster='git pull origin master'
#bash completion will do the rest below
alias git-github-clone='git clone'
# never used
# alias gm='git commit'
# alias gmm='git commit -a -m'
# alias grso='git remote show origin'
# alias gra='git remote add'
# alias grr='git remote remove'
# alias grr='git remote rm'
# alias gmmm='git commit -a -m "autocommit on $(date)"'
# alias gamm='git add -A; git commit -a -m'
# alias gmmma='git add -A; git commit -a -m "autocommit on $(date)"'
# alias gitstatus='git status'
# alias gitcommit='git commit'

#apt
#alias sagi='sudo apt-get install'
alias sagiy='sudo apt-get install -y'
alias aff='apt-file find'
alias acs='apt-cache show'
alias spsi='sudo python setup.py install'
alias affexact='bash -xc '\''apt-file find $0 | grep "/$0$"'\'''


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
alias sstatus='bash -xc '\''sudo service $0 status'\'''
alias sstart='bash -xc '\''sudo service $0 start'\'''
alias sstop='bash -xc '\''sudo service $0 stop'\'''

#misc programs
alias untar='tar axvf'
alias umount='sudo umount'
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


# Local Variables:
# mode: sh
# End:
