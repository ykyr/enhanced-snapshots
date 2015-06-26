#wget http://www.opendedup.org/downloads/SDFS-2.0.11-2.x86_64.rpm
#rpm --force -iv SDFS-2.0.11-2.x86_64.rpm
export PATH=$PATH:/sbin

aws_account_id=`curl -s http://169.254.169.254/latest/dynamic/instance-identity/document | grep accountId | cut -f2 -d: | cut -f2 -d\"`

bucketname="$aws_account_id-snapdirector-$SNAPDIRECTORNAME"

mkdir /opt/sdfs
aws s3 sync s3://$bucketname/opt/sdfs /opt/sdfs
if [ -d /opt/sdfs/volumes/s3backed0 ] ; then
  echo "Existing sdfs found on S3"
  aws s3 sync s3://$bucketname/etc/sdfs /etc/sdfs
  aws s3 sync s3://$bucketname/var/log/sdfs /var/log/sdfs
else
  echo "No sdfs on S3, so create a local one"
  mkfs.sdfs\
    --volume-name=s3backed0 \
    --volume-capacity=256TB \
    --aws-enabled=true \
    --cloud-access-key=$aws_access_key \
    --cloud-bucket-name=$bucketname \
    --cloud-secret-key=$aws_secrect_key \
    --chunk-store-encrypt=true \
    --aws-bucket-location=US
fi

mkdir -p /media/s3backed0
mount.sdfs s3backed0 /media/s3backed0/ &	

