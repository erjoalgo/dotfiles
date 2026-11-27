#!/bin/bash -x

set -euo pipefail

# Re-run this script under script(1), which records the whole session
# to LOG. Debconf dialogs keep working because script gives the run a
# real terminal, unlike a pipe through tee.
#   -a  append to LOG, so a re-run after a failure keeps the first attempt
#   -e  exit with the upgrade's exit code, not script's
#   -c  run this command instead of an interactive shell
LOG_DIR=${XDG_STATE_HOME:-${HOME}/.local/state}
LOG=${LOG:-${LOG_DIR}/bookworm-to-trixie.log}
if test -z "${IN_SCRIPT_LOG:-}"; then
    mkdir -p "$(dirname "${LOG}")"
    IN_SCRIPT_LOG=1 exec script -a -e -c "$0" "${LOG}"
fi

MIN_FREE_GB=${MIN_FREE_GB:-5}
for DIR in / /var/cache/apt/archives; do
    FREE_GB=$(df --output=avail -BG "${DIR}" | tail -1 | tr -dc 0-9)
    if test "${FREE_GB}" -lt "${MIN_FREE_GB}"; then
        echo "only ${FREE_GB}G free on ${DIR}, need ${MIN_FREE_GB}G (MIN_FREE_GB)" >&2
        exit 1
    fi
done

NEWEST_KERNEL=$(ls /boot/vmlinuz-* | sed 's|^/boot/vmlinuz-||' | sort -V | tail -1)
if test "$(uname -r)" != "${NEWEST_KERNEL}"; then
    echo "running kernel $(uname -r) but ${NEWEST_KERNEL} is installed: reboot first" >&2
    exit 1
fi

dpkg --get-selections '*'

sudo apt list '?obsolete'

sudo apt list '?config-files'
# sudo apt purge '?config-files'

sudo apt autoremove -y
sudo apt clean

# Upgrade against Debian's repos only: a third-party repo without a
# trixie release, or with a new signing key, would otherwise fail
# apt-get update and stop the upgrade.
DISABLED_SUFFIX=.disabled-for-trixie-upgrade

# third-party-repos disable: set aside sources.list.d lists with no debian.org URL
# third-party-repos restore: switch the set-aside lists to trixie and put them back
third-party-repos() {
    case "$1" in
        disable)
            for LIST in $(grep -L -P '://([a-z0-9.-]+\.)?debian\.org/' /etc/apt/sources.list.d/*.list || true); do
                sudo mv "${LIST}" "${LIST}${DISABLED_SUFFIX}"
            done
            ;;
        restore)
            for LIST in /etc/apt/sources.list.d/*.list"${DISABLED_SUFFIX}"; do
                test -e "${LIST}" || continue
                sudo sed -i 's/bookworm/trixie/g' "${LIST}"
                sudo mv "${LIST}" "${LIST%"${DISABLED_SUFFIX}"}"
            done
            ;;
        *)
            echo "usage: third-party-repos disable|restore" >&2
            return 1
            ;;
    esac
}

third-party-repos disable

sudo apt-get update
sudo apt-get upgrade --without-new-pkgs -y
sudo apt-get autoremove -y

sudo find /etc/apt/ -type f -name '*.list' -exec sed -i 's/bookworm/trixie/g' {} \;

sudo apt-get update
sudo apt-get upgrade --without-new-pkgs -y
sudo apt-get full-upgrade -y

third-party-repos restore

if ! sudo apt-get update; then
    echo "WARNING: some third-party repos failed to update (see above);" \
         "fix their trixie path or signing key, e.g. microsoft:" \
         "https://packages.microsoft.com/config/debian/13/packages-microsoft-prod.deb" >&2
fi
sudo apt-get full-upgrade -y

sudo find /etc/apt/ -name '*.bak'  -exec rm {} +

sudo apt-get install -y linux-image-amd64

sudo apt purge -y '~c'
echo "packages no longer in any repo; review, then 'sudo apt purge ~o' if none are wanted:"
sudo apt list '~o'

NEWEST_KERNEL=$(ls /boot/vmlinuz-* | sed 's|^/boot/vmlinuz-||' | sort -V | tail -1)
if test "$(uname -r)" != "${NEWEST_KERNEL}"; then
    echo "upgrade done; reboot into ${NEWEST_KERNEL} (running $(uname -r))"
fi
