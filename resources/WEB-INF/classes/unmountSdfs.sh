############################################################################
######################## script for SDFS mount #############################

### usage: sudo ./mount_sdfs.sh <SDFS volume size> <cloud bucket name> <cloud bucket location>
# all arguments except <cloud bucket location> is required for SDFS to be mounted

### passing variables from command line arguments


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
    exit 0
else
    echo 'SDFS is already unmounted'
    exit 0
fi

### restart awslogs service
service awslogs restart