### usage: sudo ./getSdfsState.sh
### return 0 in case sdfs is running 1 otherwise

sdfs_pid=`ps aux | grep "[f]use.SDFS.MountSDFS" | awk '{ print $2}'`
if [ "$sdfs_pid" != "" ]; then
    echo 'sdfs is running';
    exit 0
else
    echo 'sdfs is not running';
    exit 1
fi