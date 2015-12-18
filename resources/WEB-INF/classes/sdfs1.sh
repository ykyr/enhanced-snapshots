############################################################################
######################## script for SDFS mount #############################

### usage: sudo ./mount_sdfs.sh <SDFS volume size> <cloud bucket name> <cloud bucket location>
# all arguments except <cloud bucket location> is required for SDFS to be mounted

### passing variables from command line arguments
sdfs_volume_size="$1"
bucket_name="$2"
is_restore="${3:-false}"
location="${4:-US Standard}"
echo 'Script parameters:'
echo $1
echo $2
echo $3
echo $4

### creating SDFS file system
if [[ -e /etc/sdfs/awspool-volume-cfg.xml ]]; then
    echo 'SDFS already configured'
else
    if ($is_restore) then
        echo 'SDFS configuration is not provided!'
        exit 1
    else
        /sbin/mkfs.sdfs  --volume-name=awspool --volume-capacity=$sdfs_volume_size --aws-enabled=true --aws-aim --cloud-bucket-name=$bucket_name --aws-bucket-location=$location --local-cache-size=1GB --chunk-store-encrypt=true
    fi
fi

### creating mountpoint
if [[ ! -e /mnt/awspool ]]; then
           mkdir /mnt/awspool
        fi
        sleep 5

touch /var/log/sdfs_mount.log

### mounting SDFS file system to /mnt/awspool
sdfs_pid=`ps aux | grep "[f]use.SDFS.MountSDFS" | awk '{ print $2}'`
if [ "$sdfs_pid" != "" ]; then
    umount /mnt/awspool > /dev/null
    trap "kill $sdfs_pid 2> /dev/null" EXIT
    while kill -0 $sdfs_pid 2> /dev/null; do
     sleep 1
    done
    trap - EXIT
    echo '  SDFS sucessfully unmounted  '
    exit 1
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