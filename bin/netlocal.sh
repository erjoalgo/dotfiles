#!/bin/bash

set -euo pipefail

ALLOWED_ADDRESSES=()

while getopts "a:" OPT; do
    case ${OPT} in
        a)
            ALLOWED_ADDRESSES=(${OPTARG})
            ;;
        h)
            less "$0"
            exit 0
            ;;
        *)
            echo "unrecognized flag: ${OPT}" && exit ${LINENO}
            ;;
    esac
done
shift $((OPTIND -1))

GNAME=$(head -c29  <<< $(uuidgen | tr -d '-'))


DEBUG=false
function cleanup {
    for TABLE in iptables ip6tables; do
        MATCHING_RULES=$(sudo ${TABLE} -L OUTPUT --line-numbers |  \
                             grep "${GNAME}")
        if test ${DEBUG} = true; then
            echo "${MATCHING_RULES}"
            set -x
        fi
        RULE_NUMS=$(echo "${MATCHING_RULES}" |  \
                        grep -Po '^[0-9]+' |  \
                        sort -nr) || true
        for NUM in ${RULE_NUMS}; do
            sudo ${TABLE} -D OUTPUT "$NUM"
        done
        set +x
    done

    sudo groupdel ${GNAME}
}

trap cleanup EXIT

# Ensure group exists
sudo addgroup ${GNAME} --allow-bad-names
sudo usermod -aG ${GNAME} $USER


for ADDRESS in ${ALLOWED_ADDRESSES}; do
    HOST=$(cut -f1 -d:  <<< "${ADDRESS}")
    PORT=$(cut -f2 -d:  <<< "${ADDRESS}")
    sudo iptables -A OUTPUT -m owner --gid-owner ${GNAME} \
         -d ${HOST} -p tcp --dport ${PORT} -j ACCEPT
done
sudo iptables -A OUTPUT -m owner --gid-owner ${GNAME} -j REJECT
sudo ip6tables -A OUTPUT -m owner --gid-owner ${GNAME} -j REJECT


sudo -u "$USER" -g "${GNAME}" ${@}
