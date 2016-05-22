############################################################################
######################## script for SDFS mount #############################

### usage: sudo ./mount_sdfs.sh <SDFS volume size> <cloud bucket name> <cloud bucket location>
# all arguments except <cloud bucket location> is required for SDFS to be mounted

### passing variables from command line arguments
is_restore="${3:-false}"
echo 'Script parameters:'
echo $1

### creating mountpoint
if [[ ! -e /mnt/awspool ]]; then
           mkdir /mnt/awspool
        fi
        sleep 5

touch /var/log/sdfs_mount.log

### mounting SDFS file system to /mnt/awspool
sdfs_pid=`ps aux | grep "[f]use.SDFS.MountSDFS" | awk '{ print $2}'`
if [ "$sdfs_pid" != "" ]; then
    echo 'SDFS is already mounted'
    exit 0
else
    if ($is_restore) then
        echo 'Restoring SDFS from existed bucked'
        mount.sdfs awspool /mnt/awspool -cfr &> /var/log/sdfs_mount.log &
    else
        mount.sdfs awspool /mnt/awspool &> /var/log/sdfs_mount.log &
    fi
    tail -f /var/log/sdfs_mount.log | while read LOGLINE
    do
       [[ "${LOGLINE}" == *"Mounted Filesystem"* ]] && pkill -P $$ tail
       done
    echo '  SDFS sucessfully mounted  '
    exit 0
fi

### restart awslogs service
service awslogs restart