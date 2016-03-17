#!/bin/bash
function findiregex {
    #echo -e $(find $1 -iregex ".*$2.*")
    # dir=$1
    dir=${PWD}
    shift
    find ${dir} -iregex ".*$*.*" 
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

function plusx {
    #echo -e $(find $1 -iregex ".*$2.*")
    dest=${HOME}/bin/$(basename $1)
    abs=$(abspath "$1")
    [ -e ${dest} ] || lnabs ${abs} ${dest}
    chmod +x $1
    #re_source
}

function xargsn {
    xargs -d'\n' -L 1 $*
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

function netstattulpngrep {
    netstat -tulpn | grep "${1}"
}
