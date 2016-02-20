#!/bin/bash
function findiregex {
    #echo -e $(find $1 -iregex ".*$2.*")
    dir=$1
    shift
    find ${dir} -iregex ".*$*.*" 
}

function re_source {
    xdotool type 'source ~/.bashrc'
    xdotool key Return
}

function abspath {
     echo $(readlink -f "${1}")
}

function lnabs {
    #http://stackoverflow.com/questions/4187210/convert-relative-symbolic-links-to-absolute-symbolic-links
    relative=$1
    shift
    ln -sf "$(readlink -f "${relative}")" "$*"
}

function lnabs_t {
    target=$1
    relative=$2
    shift
    shift
    ln -s -t ${target} "$(readlink -f "${relative}")"
    }

function addsong {
    lnabs $1 /home/ernesto/Music/phone_music_sync/ 
}
function plusx {
    #echo -e $(find $1 -iregex ".*$2.*")
    dest=${HOME}/bin/$(basename $1)
    abs=$(abspath "$1")
    [ -e ${dest} ] || lnabs ${abs} ${dest}
    chmod +x $1
    #re_source
}
function greppysources {
    find /home/ernesto/Projects/ -iregex '.*py'  | xargs -d'\n' grep $*
}
function clip {
    echo $* | xs
}
function addmusic {
    lnabs $1 /home/ernesto/Music/phone_music_sync/ 
    }
function playmusic {
    findiregex /home/ernesto/Music "$*" | xargsn mpg321
}
function xargsn {
    xargs -d'\n' -L 1 $*
    }

function cdls {
    cd $1 && ls 
    }

function replace_when_differ {
    if diff $1 $2;
    then
	true
    else
	cp $1 $2
    fi
}

function add_single_file {
    if diff $1 $2;
    then
	true
    else
	cp $1 $2
    fi
}


#http://superuser.com/questions/150117/how-to-get-parent-pid-of-a-given-process-in-gnu-linux-from-command-line
ppid () { ps -p ${1:-$$} -o ppid=; }


# http://www.cyberciti.biz/faq/linux-random-password-generator/
function genpasswd {
    local l=$1;
    [ "$l" == "" ] && l=16;
    tr -dc A-Za-z0-9_ < /dev/urandom | head -c ${l} | xargs;
}
function test_port	{
    HOST="${1}"
    PORT="${2}"
    # http://stackoverflow.com/questions/9609130/quick-way-to-find-if-a-port-is-open-on-linux
    # echo -e "GET / HTTP/1.0\n" >&6
    # cat <&6
    exec 6<>/dev/tcp/"${HOST}/${PORT}"
}
function straceall {
    set -x
    NAME="${1}"
    shift
    strace $* $(pidof "${NAME}" | sed 's/\([0-9]*\)/-p \1/g')
}

function psauxgrep {
    ps aux|grep "${1}"
}
