#!/bin/bash -x

set -euo pipefail

MODE="client"
MOUNT_POINT="${HOME}/mnt/docs"

while getopts "csh:" OPT; do
    case ${OPT} in
        s)
            MODE="server"
            ;;
        c)
            MODE="client"
            ;;
        h)
            less $0
            exit 0
            ;;
    esac
done
shift $((OPTIND -1))

if test "${MODE}" = "client"; then
    # fail early before prompts if mount point is unavailable
    mkdir -p "${MOUNT_POINT}"{,.local}
    read -p"Enter backup server hostname: " SSHFS_SERVER
    read -p"Enter backup server port: " SSHFS_PORT

    read -p"Enter local backup server hostname: " SSHFS_SERVER_LOCAL
    read -p"Enter local backup server port: " SSHFS_PORT_LOCAL
    SSHFS_PATH=${SSHFS_PATH:-docs}
    USERNAME=${USERNAME:-ealfonso}

    sudo insert-text-block  \
         '# a62a6d16-6dae-4fdd-b42c-94d72bd8ec60-sshfs-backup-automount' \
         /etc/fstab<<EOF
${USERNAME}@${SSHFS_SERVER}:${SSHFS_PATH} ${MOUNT_POINT}.remote fuse.sshfs _netdev,user,idmap=user,transform_symlinks,identityfile=/home/${USERNAME}/.ssh/id_rsa,allow_other,default_permissions,port=${SSHFS_PORT} 0
${USERNAME}@${SSHFS_SERVER_LOCAL}:${SSHFS_PATH} ${MOUNT_POINT} fuse.sshfs _netdev,user,idmap=user,transform_symlinks,identityfile=/home/${USERNAME}/.ssh/id_rsa,allow_other,default_permissions,port=${SSHFS_PORT_LOCAL} 0
EOF
    sudo mount -av
elif test "${MODE}" = "server"; then
    sudo apt-get install -y rsnapshot rsync
    sudo rm -f /etc/rsnapshot.conf
    USB_DRIVE_DOCS_PATH=${USB_DRIVE_DOCS_PATH:-/home/ealfonso/mnt/575841314142384E5A374539/docs}
    LOCAL_DOCS_PATH=${LOCAL_DOCS_PATH:-/home/ealfonso/docs}
    sudo insert-text-block  \
        '# e55f88e5-e2b7-466b-8b24-1c0c55165985-rsnapshot-usb-backups'  \
        /etc/rsnapshot.conf<<EOF
#################################################
# rsnapshot.conf - rsnapshot configuration file #
#################################################
#                                               #
# PLEASE BE AWARE OF THE FOLLOWING RULE:        #
#                                               #
# This file requires tabs between elements      #
#                                               #
#################################################

#######################
# CONFIG FILE VERSION #
#######################

config_version	1.2

###########################
# SNAPSHOT ROOT DIRECTORY #
###########################

# All snapshots will be stored under this root directory.
#
snapshot_root	${USB_DRIVE_DOCS_PATH}

# If no_create_root is enabled, rsnapshot will not automatically create the
# snapshot_root directory. This is particularly useful if you are backing
# up to removable media, such as a FireWire or USB drive.
#
no_create_root	1

#################################
# EXTERNAL PROGRAM DEPENDENCIES #
#################################

# LINUX USERS:   Be sure to uncomment "cmd_cp". This gives you extra features.
# EVERYONE ELSE: Leave "cmd_cp" commented out for compatibility.
#
# See the README file or the man page for more details.
#
cmd_cp		/bin/cp

# uncomment this to use the rm program instead of the built-in perl routine.
#
cmd_rm		/bin/rm

# rsync must be enabled for anything to work. This is the only command that
# must be enabled.
#
cmd_rsync	/usr/bin/rsync

# Uncomment this to enable remote ssh backups over rsync.
#
#cmd_ssh	/usr/bin/ssh

# Comment this out to disable syslog support.
#
cmd_logger	/usr/bin/logger

# Uncomment this to specify the path to "du" for disk usage checks.
# If you have an older version of "du", you may also want to check the
# "du_args" parameter below.
#
cmd_du		/usr/bin/du

# Uncomment this to specify the path to rsnapshot-diff.
#
#cmd_rsnapshot_diff	/usr/bin/rsnapshot-diff

# Specify the path to a script (and any optional arguments) to run right
# before rsnapshot syncs files
#
#cmd_preexec	/path/to/preexec/script

# Specify the path to a script (and any optional arguments) to run right
# after rsnapshot syncs files
#
#cmd_postexec	/path/to/postexec/script

# Paths to lvcreate, lvremove, mount and umount commands, for use with
# Linux LVMs.
#
#linux_lvm_cmd_lvcreate	/sbin/lvcreate
#linux_lvm_cmd_lvremove	/sbin/lvremove
#linux_lvm_cmd_mount	/bin/mount
#linux_lvm_cmd_umount	/bin/umount

#########################################
#     BACKUP LEVELS / INTERVALS         #
# Must be unique and in ascending order #
# e.g. alpha, beta, gamma, etc.         #
#########################################

retain	alpha	6
retain	beta	7
retain	gamma	4
#retain	delta	3

############################################
#              GLOBAL OPTIONS              #
# All are optional, with sensible defaults #
############################################

# Verbose level, 1 through 5.
# 1     Quiet           Print fatal errors only
# 2     Default         Print errors and warnings only
# 3     Verbose         Show equivalent shell commands being executed
# 4     Extra Verbose   Show extra verbose information
# 5     Debug mode      Everything
#
verbose		2

# Same as "verbose" above, but controls the amount of data sent to the
# logfile, if one is being used. The default is 3.
# If you want the rsync output, you have to set it to 4
#
loglevel	3

# If you enable this, data will be written to the file you specify. The
# amount of data written is controlled by the "loglevel" parameter.
#
# logfile	/var/log/rsnapshot.log

# If enabled, rsnapshot will write a lockfile to prevent two instances
# from running simultaneously (and messing up the snapshot_root).
# If you enable this, make sure the lockfile directory is not world
# writable. Otherwise anyone can prevent the program from running.
#
lockfile	/var/run/rsnapshot.pid

# By default, rsnapshot check lockfile, check if PID is running
# and if not, consider lockfile as stale, then start
# Enabling this stop rsnapshot if PID in lockfile is not running
#
#stop_on_stale_lockfile		0

# Default rsync args. All rsync commands have at least these options set.
#
#rsync_short_args	-a
#rsync_long_args	--delete --numeric-ids --relative --delete-excluded

# ssh has no args passed by default, but you can specify some here.
#
#ssh_args	-p 22

# Default arguments for the "du" program (for disk space reporting).
# The GNU version of "du" is preferred. See the man page for more details.
# If your version of "du" doesn't support the -h flag, try -k flag instead.
#
#du_args	-csh

# If this is enabled, rsync won't span filesystem partitions within a
# backup point. This essentially passes the -x option to rsync.
# The default is 0 (off).
#
#one_fs		0

# The include and exclude parameters, if enabled, simply get passed directly
# to rsync. If you have multiple include/exclude patterns, put each one on a
# separate line. Please look up the --include and --exclude options in the
# rsync man page for more details on how to specify file name patterns.
# 
#include	???
#include	???
#exclude	???
#exclude	???

# The include_file and exclude_file parameters, if enabled, simply get
# passed directly to rsync. Please look up the --include-from and
# --exclude-from options in the rsync man page for more details.
#
#include_file	/path/to/include/file
#exclude_file	/path/to/exclude/file

# If your version of rsync supports --link-dest, consider enabling this.
# This is the best way to support special files (FIFOs, etc) cross-platform.
# The default is 0 (off).
#
#link_dest	0

# When sync_first is enabled, it changes the default behaviour of rsnapshot.
# Normally, when rsnapshot is called with its lowest interval
# (i.e.: "rsnapshot alpha"), it will sync files AND rotate the lowest
# intervals. With sync_first enabled, "rsnapshot sync" handles the file sync,
# and all interval calls simply rotate files. See the man page for more
# details. The default is 0 (off).
#
#sync_first	0

# If enabled, rsnapshot will move the oldest directory for each interval
# to [interval_name].delete, then it will remove the lockfile and delete
# that directory just before it exits. The default is 0 (off).
#
#use_lazy_deletes	0

# Number of rsync re-tries. If you experience any network problems or
# network card issues that tend to cause ssh to fail with errors like
# "Corrupted MAC on input", for example, set this to a non-zero value
# to have the rsync operation re-tried.
#
#rsync_numtries 0

# LVM parameters. Used to backup with creating lvm snapshot before backup
# and removing it after. This should ensure consistency of data in some special
# cases
#
# LVM snapshot(s) size (lvcreate --size option).
#
#linux_lvm_snapshotsize	100M

# Name to be used when creating the LVM logical volume snapshot(s).
#
#linux_lvm_snapshotname	rsnapshot

# Path to the LVM Volume Groups.
#
#linux_lvm_vgpath	/dev

# Mount point to use to temporarily mount the snapshot(s).
#
#linux_lvm_mountpath	/path/to/mount/lvm/snapshot/during/backup

###############################
### BACKUP POINTS / SCRIPTS ###
###############################

# LOCALHOST
backup	${LOCAL_DOCS_PATH}		localhost/
# backup	/etc/		localhost/
# backup	/usr/local/	localhost/
#backup	/var/log/rsnapshot		localhost/
#backup	/etc/passwd	localhost/
#backup	/home/foo/My Documents/		localhost/
#backup	/foo/bar/	localhost/	one_fs=1, rsync_short_args=-urltvpog
#backup_script	/usr/local/bin/backup_pgsql.sh	localhost/postgres/
# You must set linux_lvm_* parameters below before using lvm snapshots
#backup	lvm://vg0/xen-home/	lvm-vg0/xen-home/

# EXAMPLE.COM
#backup_exec	/bin/date "+ backup of example.com started at %c"
#backup	root@example.com:/home/	example.com/	+rsync_long_args=--bwlimit=16,exclude=core
#backup	root@example.com:/etc/	example.com/	exclude=mtab,exclude=core
#backup_exec	ssh root@example.com "mysqldump -A > /var/db/dump/mysql.sql"
#backup	root@example.com:/var/db/dump/	example.com/
#backup_exec	/bin/date "+ backup of example.com ended at %c"

# CVS.SOURCEFORGE.NET
#backup_script	/usr/local/bin/backup_rsnapshot_cvsroot.sh	rsnapshot.cvs.sourceforge.net/

# RSYNC.SAMBA.ORG
#backup	rsync://rsync.samba.org/rsyncftp/	rsync.samba.org/rsyncftp/

EOF
    sudo insert-text-block '# 8b41c80e-a8c1-4596-a7d9-035accc811a8-rsnapshot-crontab' \
         /etc/cron.d/rsnapshot \
         <<EOF
0 */4 * * *     /usr/bin/rsnapshot alpha
50 23 * * *     /usr/bin/rsnapshot beta
00 22 1 * *     /usr/bin/rsnapshot delta
EOF
fi
