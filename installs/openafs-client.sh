#!/bin/bash -x

set -euo pipefail

cd "$(realpath $(dirname "${BASH_SOURCE[0]}"))"

while getopts "c:i:g:h" OPT; do
    case ${OPT} in
    c)
        THIS_CELL=${OPTARG}
        ;;
    i)
        THIS_CELL_IP=${OPTARG}
        ;;
    g)
        AFS_CACHE_GB=${OPTARG}
        ;;
    h)
        less $(basename "$0")
        exit 0
        ;;
    *)
        echo "unrecognized flag: ${OPT}" && exit ${LINENO}
        ;;
    esac
done
shift $((OPTIND -1))

AFS_CACHE_GB=${AFS_CACHE_GB:-20}

if ! command -v bc; then
    sudo apt-get install -y bc
fi

AFS_CACHE_KB=$(bc <<< "scale=2; ${AFS_CACHE_GB} * 1024 ^ 2")

sudo apt-get install -y linux-headers-$(uname -r)

test -n "${THIS_CELL:-}"
sudo tee /etc/openafs/ThisCell <<< "${THIS_CELL}"

if test -n "${THIS_CELL_IP:-}"; then
    insert-text-block \
        '# 126de658-b186-4abe-a579-60f78f0365dc-openafs-host-ip'  \
        /etc/hosts<<EOF
${THIS_CELL_IP}	${THIS_CELL}
EOF
fi

sudo debconf-set-selections <<EOF
# Size of AFS cache in kB:
openafs-client  openafs-client/cachesize        string  ${AFS_CACHE_KB}
# AFS cell this workstation belongs to:
openafs-client  openafs-client/thiscell string  ${THIS_CELL}

# Look up AFS cells in DNS?
openafs-client  openafs-client/afsdb    boolean true
# DB server host names for your home cell:
openafs-client  openafs-client/cell-info        string
# Encrypt authenticated traffic with AFS fileserver?
openafs-client  openafs-client/crypt    boolean true
# Dynamically generate the contents of /afs?
# Choices: Yes, Sparse, No
openafs-client  openafs-client/dynroot  select  Yes
# Use fakestat to avoid hangs when listing /afs?
openafs-client  openafs-client/fakestat boolean true
# Run Openafs client now and at boot?
openafs-client  openafs-client/run-client       boolean true


# Add locations of default Kerberos servers to /etc/krb5.conf?
krb5-config     krb5-config/add_servers boolean false
# for internal use
krb5-config     krb5-config/add_servers_realm   string  ${THIS_CELL}
# Default Kerberos version 5 realm:
krb5-config     krb5-config/default_realm       string  ${THIS_CELL^^}

# Administrative server for your Kerberos realm:
# krb5-config     krb5-config/admin_server        string
# Kerberos servers for your realm:
# krb5-config     krb5-config/kerberos_servers    string
# For internal use only
krb5-config     krb5-config/read_conf   boolean true
EOF

sudo DEBIAN_FRONTEND=noninteractive apt-get install -y openafs-{modules-dkms,client,krb5}  \
     krb5-{config,user} libpam-krb5

test -n "${THIS_CELL:-}"
sudo tee /etc/openafs/ThisCell <<< "${THIS_CELL}"

sudo DEBIAN_FRONTEND=noninteractive dpkg-reconfigure openafs-client

CELLSERVDB=/etc/openafs/CellServDB

if ! grep -F ">{THIS_CELL}" "${CELLSERVDB}" || true; then
    IP_ADDR=$(dig +short ${THIS_CELL})
    sudo tee -a "${CELLSERVDB}" <<EOF
>${THIS_CELL}
${IP_ADDR} #${THIS_CELL}
EOF
fi


REALM_BLOCK=$(cat<<EOF
	${THIS_CELL^^} = {
		kdc = ${THIS_CELL}
		admin_server = ${THIS_CELL}
	}
EOF
)
REALM_INDICATOR_LINE=$(head -1  <<< "${REALM_BLOCK}")

KRB5_CONF=/etc/krb5.conf

if ! grep -F "${REALM_INDICATOR_LINE}" "${KRB5_CONF}" || true; then
    echo "installing realm block..."
    sudo insert-text-block '# EUZHu5M9rmPekPHNsRDCV7H3mL0s95iB-add-openafs-realm' \
         "${KRB5_CONF}" -r "^.realms." <<< "${REALM_BLOCK}"
    grep -F "${REALM_INDICATOR_LINE}" "${KRB5_CONF}"
fi

ln -fs "/afs/${THIS_CELL}/" ${HOME}/afs

ping -c3 "${THIS_CELL}"
