#!/bin/bash
export VISUAL='emacsclient'
export EDITOR='emacsclient'

# ERNESTO="/home/ernesto"
ERNESTO="${HOME}"

# PROJECTS="${ERNESTO}/projects"
# PORTABLE_ROOT="${ERNESTO}/portable_root"
# REPOS="${PORTABLE_ROOT}/repos"

REPOS="${HOME}/repos"
#BASH_FILES="${REPOS}/bash"
BASH_FILES="${HOME}"

INIT_FILES="${PROJECTS}/init_files"
BIN="${PROJECTS}/bin"
PROGRAMS="${ERNESTO}/programs"

UNIX_UTILS="${REPOS}/unix_utils"

export PATH=$PATH:/sbin:${HOME}/bin:${UNIX_UTILS}:${PROGRAMS}/cc0/bin/

# export PYTHONPATH=${PYTHONPATH}:${PROJECTS}/bin:${PROJECTS}/:${PROJECTS}/python_path
# export PYTHONSTARTUP=${INIT_FILES}/.pythonrc.py

export PYTHONSTARTUP=${HOME}/.pythonrc.py
source ${BASH_FILES}/.bash_aliases

shopt -s histappend
export HISTSIZE=100000
export HISTFILESIZE=100000000
export LISP=sbcl



source ${BASH_FILES}/my_bash_funs.sh
export -f xargsn
export -f lnabs
export -f replace_when_differ

PS1="\h$"

#this is for cuddlefish
FIREFOX_ADDON_SDK=${PROGRAMS}/addon-sdk-1.17
CUDDLEFISH_ROOT=${FIREFOX_ADDON_SDK}
export CUDDLEFISH_ROOT

export PYTHONPATH=${PYTHONPATH}:${CUDDLEFISH_ROOT}/python-lib
export PATH=${PATH}:${CUDDLEFISH_ROOT}/bin

# PATH=${PATH}:${PROGRAMS}/adt-bundle-linux-x86_64-20140321/sdk/platform-tools
# PATH=${PATH}:${PROGRAMS}/adt-bundle-linux-x86_64-20140321/sdk/tools/

# go
export GOROOT=${PROGRAMS}/go
export GOPATH=${REPOS}/gopath
export PATH=$PATH:$GOPATH/bin:$GOROOT/bin
export PATH=$PATH:${PROGRAMS}/apache-maven-3.3.9/bin
export JAVA_HOME=/usr/lib/jvm/jdk-8-oracle-x64

export PATH
export GOPATH
# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=100000
HISTFILESIZE=200000

# check the window size after each command and, if necessary, update the values of LINES and COLUMNS.
shopt -s checkwinsize


alias ls='ls --color=auto'
alias grep='grep --color=auto'

#TODO cleaup this file
