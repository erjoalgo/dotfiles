#!/bin/bash

#emacs editor
export VISUAL='emacsclient'
export EDITOR='emacsclient'

#bash prompt
PS1="\h$"


REPOS="${HOME}/repos"
PROGRAMS="${HOME}/programs"
UNIX_UTILS="${REPOS}/unix_utils"

source .bash_aliases #should already be sourced by normal bashrc
source .my_bash_funs.sh #lnabs, findiregex, etc


#unix utils
export PATH=$PATH:/sbin:/usr/sbin:${HOME}/bin:${UNIX_UTILS}

#lisp
export LISP=sbcl

#python
export PYTHONSTARTUP=${HOME}/.pythonrc.py

# go
export GOROOT=${PROGRAMS}/go
export GOPATH=${REPOS}/gopath
export PATH=$PATH:$GOPATH/bin:$GOROOT/bin

#java/maven
export PATH=$PATH:${PROGRAMS}/apache-maven-3.3.9/bin
export JAVA_HOME=/usr/lib/jvm/jdk-8-oracle-x64

export PATH
export GOPATH


#just source them all
source ~/.my-bash-completions/sagiy

#history control from multiple terminals. taken from internet. not really working
# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth
# append to the history file, don't overwrite it
shopt -s histappend
# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
export HISTSIZE=100000
export HISTFILESIZE=100000000
# check the window size after each command and, if necessary, update the values of LINES and COLUMNS.
shopt -s checkwinsize
