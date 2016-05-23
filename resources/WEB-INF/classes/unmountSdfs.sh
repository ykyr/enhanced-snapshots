############################################################################
######################## script for SDFS unmount #############################


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