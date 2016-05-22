############################################################################
######################## script for SDFS mount #############################

### usage: sudo ./mount_sdfs.sh <SDFS volume size> <cloud bucket name> <cloud bucket location>
# all arguments except <cloud bucket location> is required for SDFS to be mounted

### passing variables from command line arguments
sdfs_volume_size="$1"
bucket_name="$2"
location="${3:-US Standard}"
localCacheSize="${4:-1GB}"
echo 'Script parameters:'
echo $1
echo $2
echo $3
echo $4


### creating SDFS file system
if [[ -e /etc/sdfs/awspool-volume-cfg.xml ]]; then
    echo 'SDFS already configured'
    exit 0
else
    /sbin/mkfs.sdfs  --volume-name=awspool --volume-capacity=$sdfs_volume_size --aws-enabled=true --aws-aim --cloud-bucket-name=$bucket_name --aws-bucket-location=$location --local-cache-size=$localCacheSize --chunk-store-encrypt=true
    echo 'SDFS is configured'
    exit 0
fi
