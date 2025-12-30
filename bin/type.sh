#!/bin/bash -x

set -euo pipefail

. ${HOME}/afs/home/${USER}/.profile || true

MODE=automation
SPECULOS_DISPLAY_OPT=()

while getopts "p:s:d:xcah" OPT; do
    case ${OPT} in
    s)
        SEED_FILE=${OPTARG}
        ;;
    # optional
    d)
        DELAY_SECS=${OPTARG}
        ;;
    x)
        MODE=xdotool
        ;;
    c)
        MODE=curl
        ;;
    a)
        MODE=automation
        ;;
    p)
        PORT=${OPTARG}
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

function test-port () {
    PROTOCOL=${PROTOCOL:-tcp};
    if test $# = 1; then
        HOST=127.0.0.1;
    else
        HOST="${1}" && shift;
    fi;
    PORT=${1} && shift;
    if test ${PROTOCOL} = udp; then
        nc -vz -u ${HOST} ${PORT};
    else
        exec 6<> /dev/${PROTOCOL}/${HOST}/${PORT};
    fi
}

function get-random-valid-port {
    while true; do
        PORT=$(shuf -i 2000-65000 -n 1)
        if ! exec 6<> /dev/tcp/localhost/${PORT} > /dev/null 2>&1; then
            # found unsued port
            echo "${PORT}"
            break;
        fi
    done
}

if test -z "${PORT:-}"; then
    PORT=$(get-random-valid-port)
fi


PASS_ID=${1:-} && shift || true

if test "${MODE}" = automation; then
    SPECULOS_DISPLAY_OPT=("--display=headless")
    BROWSE=false
elif test "${MODE}" = xdotool; then
    DELAY_SECS=${DELAY_SECS:-0.05}
fi

function validate-args {
    if test -z "${SEED_FILE:-}"; then
        echo "no seed file defined" && exit ${LINENO}
    fi

    if test -z "${PASS_ID:-}"; then
        URL=$(current-url)
        PASS_ID=$(url-to-pass-id "${URL}")
        echo "${PASS_ID}" >&2
    fi
    if test -z "${PASS_ID:-}"; then
        echo "missing pass id" && exit ${LINENO}
    fi
    if ! test -e "${SEED_FILE:-}"; then
        echo "seed file doesn't exist" && exit ${LINENO}
    fi

    EXT="${SEED_FILE##*.}"
    if ! grep -i gpg <<< "${EXT}"; then
        echo "seed file missing gpg extension" && exit ${LINENO}
    fi
}


function press {
    KEY=${1} && shift
    TIMES=${1:-1}
    echo "moving ${KEY} ${TIMES} times" >&2
    case "${KEY}" in
        Left)
            BUTTON="left"
            BUTTON_NUMBERS=(1)
            ;;
        Right)
            BUTTON="right"
            BUTTON_NUMBERS=(2)
            ;;
        Left+Right)
            BUTTON="both"
            BUTTON_NUMBERS=(1 2)
            ;;
        *)
            echo "unknown key: ${KEY}" >&2
            return ${LINENO}
            ;;
    esac
    for _ in $(seq "${TIMES}"); do
        if test "${MODE:-}" = xdotool; then
            echo "DDEBUG TRACE type.sh t3ve"
            xdotool key "${KEY}"
            sleep "${DELAY_SECS}"
        elif test "${MODE:-}" = curl; then
            curl -d '{"action":"press-and-release"}'  \
                 "http://127.0.0.1:${PORT}/button/${BUTTON}"
        elif test "${MODE:-}" = automation; then
            FIRST=true
            for PRESS in true false; do
                for BUTTON_NUMBER in "${BUTTON_NUMBERS[@]}"; do
                    TEXT=$'\n'
                    if test "${FIRST}" = true; then
                        TEXT=$'\n\n'
                        FIRST=false
                    else
                        TEXT=$'\n';
                    fi
                    TEXT+="[\"button\", ${BUTTON_NUMBER}, ${PRESS}]"
                    ACTIONS+=("${TEXT}")
                done
            done
        else
            echo "unsupported input mode: ${MODE}" >2&
            exit ${LINENO}
        fi
    done
}

function exec-automation {
    ACTION_DATA=$(cat <<EOF
{
  "version": 1,
  "rules": [
    {
    "conditions": [[ "seen", false ]],
    "actions": [
        [ "setbool", "seen", true ],
        $(IFS=','; echo "${ACTIONS[*]}")
      ]
    }
  ]
}
EOF
)
    ledger-menu start "${SEED_FILE}" --automation file:<(echo "${ACTION_DATA}")
}

function left {
    press Left ${*}
}

function right {
    press Right ${*}
}

function both {
    press Left+Right ${*}
}

function section {
    # echo -e "\n\n"
    echo ${*} >&2
}

# https://unix.stackexchange.com/questions/92447/

chr() {
  [ "$1" -lt 256 ] || return 1
  printf "\\$(printf '%03o' "$1")"
}

ord() {
  LC_CTYPE=C printf '%d' "'$1"
}

function move-to-char {
    local CHAR
    local LAST_CHAR
    CHAR=${1} && shift
    LAST_CHAR=${1} && shift
    DIFF=$(expr $(ord "${CHAR}") - $(ord "${LAST_CHAR}")) || true
    if test ${DIFF} -lt 0; then
        left $(( - ${DIFF}))
    else
        right ${DIFF}
    fi
}

function type-string {
    PASS_ID=${1} && shift
    echo "typing ${PASS_ID}" >&2
    LAST_CHAR='a'
    for CHAR in $(grep -o . <<< "${PASS_ID}"); do
        if  grep '[a-z]' <<< "${CHAR}"; then
            move-to-char "${CHAR}" "${LAST_CHAR}"
            both
            LAST_CHAR=${CHAR}
        else
            STRING=$(cat <<'EOF'
0123456789 '"`&/?!:;.,~*$=+-[](){}<>\_#@|%
EOF
)
            if ! OFFSET=$(grep -boF "${CHAR}" <<< "${STRING}" | cut -f1 -d:); then
                echo "unsupported character: ${CHAR}" >&2
                exit ${LINENO}
            fi
            move-to-char "a" "${LAST_CHAR}"
            select-menu charset-select
            select-menu charset-numbers
            right ${OFFSET}
            both
            left ${OFFSET}
            select-menu charset-select
            select-menu charset-lowercase
            LAST_CHAR='a'
        fi
    done
    move-to-char "a" "${LAST_CHAR}"
}

function select-menu {
    SELECTION=${1} && shift
    case "${SELECTION}" in
        new-pass)
            # assume we are at "manage passwords for your device"
            section "entering the 'new password' screen"
            right 2 # new password
            both
            both # charset lowercase
            ;;
        show-pass)
            # assumes we are at "type password",
            # finish at show-password screen 2/2
            # press "both" to return to "type password"
            INDEX=${1:-0}
            section "entering the 'show password' screen"
            right 1
            both
            right "${INDEX}" # scan to the password
            both
            ;;
        capture-pass)
            PASS=$(MODE=curl last-event-text)
            export PASS
            echo "${PASS}"
            ;;
        charset-lowercase)
            # assume we are at "new password -> keyboard selection -> charset selection"
            both
            ;;
        charset-numbers)
            # assume we are at "new password -> keyboard selection -> charset selection"
            right 2
            both
            ;;
        charset-select)
            left
            both
            ;;
        checkmark)
            # asssues we are at "a", finish at "type password"
            left 2
            both # checkmark
            both # "new password created"
            ;;
        init-to-which-action-menu)
            # skip the disclaimer
            right 4 # "I understand"
            both

            # querty
            both

            # manage password
            right 1 # tap to manage
            both
            ;;
        *)
            echo "unknown select-menu option" >&2
            exit ${LINENO}
            ;;
    esac
}


function build-speculos {
    sudo apt install -y \
         cmake gcc-arm-linux-gnueabihf libc6-dev-armhf-cross gdb-multiarch \
         python3-pyqt5 python3-construct python3-flask-restful python3-jsonschema \
         python3-mnemonic python3-pil python3-pyelftools python3-requests \
         qemu-user-static libvncserver-dev
    URL=/afs/asus.erjoalgo.com/home/ealfonso/git-bare/speculos
    REPO=${HOME}/git/$(basename "${URL}")
    test -d "${REPO}" || git clone "${URL}" "${REPO}"
    cd "${REPO}"
    cmake -B build/ -DCMAKE_BUILD_TYPE=Debug -DWITH_VNC=1 -S .
    pip3 install .
    which speculos
}

function build-speculos-app {
    APP_DIR=${1} && shift
    APP_ELF=${1} && shift
    pushd .
    cd "${APP_DIR}"
    ELF=$(realpath -s --relative-to=. "${APP_ELF}")
    rm -f "${ELF}"

    docker run --rm -ti  \
           --user "$(id -u):$(id -g)" \
           -v "$(realpath .):/app" \
           ghcr.io/ledgerhq/ledger-app-builder/ledger-app-dev-tools:latest \
           make DEBUG=1
    test -e "${ELF}"
    test -e "${APP_ELF}"
    echo "pulled $(pwd)/$(basename ${ELF})" >&2
    popd
}

function build-app-passwords {
    REPO=${HOME}/git/app-passwords
    if ! test -d "${REPO}"; then
        URL=https://github.com/LedgerHQ/app-passwords
        git clone "${URL}" "${REPO}"
    fi
    APP_ELF="${REPO}/bin/app.elf"
    if test ! -e "${APP_ELF}" -o -n "${FORCE:-}"; then
        build-speculos-app "${REPO}" "${APP_ELF}"
    fi
    test -e "${APP_ELF}"
}

function log-pass-id {
    PASS_FILE="${SEED_FILE}.passwords"
    section "logging pass id ${PASS_ID} into ${PASS_FILE}"
    echo "${PASS_ID}" >> "${PASS_FILE}"
}

function modify-speculos {
    # TODO ...
    SPECULOS_INIT=$(python3 -c 'import speculos; print(speculos.__file__)')
    SPECULOS_MAIN="$(dirname ${SPECULOS_INIT})/main.py"
    test -e "${SPECULOS_MAIN}"
}



function ledger-menu {
    SELECTION=${1} && shift
    case "${SELECTION}" in
        build-all)
            command -v speculos || build-speculos
            test -e "${APP_ELF:-}" || build-app-passwords
            ;;

        start)
            section "starting ledger"
            SEED_FILE=${1} && shift
            test -e "${APP_ELF}"
            set +x
            env SMLL="$(gpg --decrypt --no-symkey-cache --batch ${SEED_FILE})" \
                speculos "${APP_ELF}" "${SPECULOS_DISPLAY_OPT[@]}" \
                "--api-port=${PORT}" ${*} &
            set -x
            while ! test-port localhost ${PORT}; do
                sleep 1
            done
            ;;

        kill)
            WAS_RUNNING=false
            while pgrep speculos; do
                WAS_RUNNING=true
                pkill speculos
                sleep 1
            done
            while true; do
                BREAK=true
                for CONTAINER_NAME in  \
                    $(docker ps| grep 'speculos.* Up ' | rev | cut -f1 -d' ' | rev); do
                    docker stop "${CONTAINER_NAME}"
                    BREAK=false
                done
                if test ${BREAK} = true; then
                    break
                fi
                sleep 1
            done

            while test-port localhost ${PORT}; do
                sleep 1
            done
            ;;

        focus)
            section "focusing ledger"
            until wmctrl -a "Ledger Nano S"; do
                echo "waiting for ledger window" >&2
                sleep 1
            done
            ;;

        browse)
            x-www-browser http://localhost:${PORT} &
            ;;

        *)
            echo "unknown select-menu option" >&2
            exit ${LINENO}
            ;;
    esac
}


function last-event-text {
    curl -s "http://127.0.0.1:${PORT}/events" |  \
        jq --raw-output ".events | map(.text) | .[]"  |  \
        tail -2 |  \
        tr -d '\n'
}

function clipboard {
    TEXT=${1} && shift
    x-service-curl /clipboard -d "${TEXT}"
}

function current-url {
    curl localhost:19615/tabs/current/url
}

function url-to-pass-id {
    URL=${1} && shift
    HOST=$(URL=$URL python - <<EOF
import os
from urllib.parse import urlparse

uri = os.environ["URL"]
result = urlparse(uri)
host = result.netloc.split('@')[-1]
pass_id = host.split('.')[-2]
print(pass_id)
EOF
)
    echo "${HOST}"
}

function main {
    validate-args
    ledger-menu build-all

    if test "${MODE}" != automation; then
        ledger-menu kill
        if ! pgrep speculos; then
            ledger-menu start "${SEED_FILE}"
        fi
    fi

    if test "${MODE}" = xdotool; then
        ledger-menu focus
    fi
    select-menu init-to-which-action-menu
    select-menu new-pass
    type-string "${PASS_ID}"
    select-menu checkmark
    select-menu show-pass
    if test "${MODE}" = automation; then
        exec-automation
    fi
    # PASS=$(select-menu capture-pass)
    select-menu capture-pass
    test -n "${PASS:-}"
    clipboard "${PASS}"
    log-pass-id
    if test "${BROWSE:-}" = true; then
        ledger-menu browse
    fi
    echo "${PASS_ID}"
    echo "${PASS}"

    pkill speculos &
    disown
}


main
