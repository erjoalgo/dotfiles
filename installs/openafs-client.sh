#!/bin/bash -x

set -euo pipefail

cd "$(realpath $(dirname "${BASH_SOURCE[0]}"))"

while getopts "c:g:h" OPT; do
    case ${OPT} in
    c)
        THIS_CELL=${OPTARG}
        ;;
    g)
        AFS_CACHE_GB=${OPTARG}
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

AFS_CACHE_GB=${AFS_CACHE_GB:-20}

if ! command -v bc; then
    sudo apt-get install -y bc
fi

AFS_CACHE_KB=$(bc <<< "scale=2; ${AFS_CACHE_GB} * 1024 ^ 2")

sudo apt-get install -y linux-headers-$(uname -r)

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

sudo DEBIAN_FRONTEND=noninteractive dpkg-reconfigure openafs-client

CELLSERVDB=/etc/openafs/CellServDB

if ! grep -F "^>{THIS_CELL}" "${CELLSERVDB}"; then
    IP_ADDR=$(dig +short ${THIS_CELL})
    sudo tee -a "${CELLSERVDB}" <<EOF
>${THIS_CELL}
${IP_ADDR} #${THIS_CELL}
EOF
fi

ln -s "/afs/${THIS_CELL}/" ${HOME}/afs

ping -c3 "${THIS_CELL}"
