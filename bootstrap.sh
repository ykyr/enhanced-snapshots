sudo yum install -y nodejs npm git s3cmd --enablerepo=epel
curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.25.4/install.sh | bash
nvm install 0.12
wget http://www.opendedup.org/downloads/SDFS-2.0.11-2.x86_64.rpm
rpm --force -iv SDFS-2.0.11-2.x86_64.rpm


aws_account_id=`curl -s http://169.254.169.254/latest/dynamic/instance-identity/document | grep accountId | cut -f2 -d: | cut -f2 -d\"`

mkdir /opt/sdfs
aws s3 sync s3://$aws_account_id-snapdirector/opt/sdfs /opt/sdfs
if [ -d /opt/sdfs/volumes/s3backed0 ] ; then
  aws s3 sync s3://$aws_account_id-snapdirector/etc/sdfs /etc/sdfs
  aws s3 sync s3://$aws_account_id-snapdirector/var/log/sdfs /var/log/sdfs
else
	mkfs.sdfs\
		--volume-name=s3backed0 \
        --volume-capacity=256TB \
        --aws-enabled=true \
        --cloud-access-key=AKIAI5OLTIAOOMBMAW4Q \
        --cloud-bucket-name=$aws_account_id-snapdirector \
        --cloud-secret-key=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 \
        --chunk-store-encrypt=true \
        --aws-bucket-location=US
fi

mkdir -p /media/s3backed0
mount.sdfs s3backed0 /media/s3backed0/ &	
python26 /usr/bin/s3cmd get --force s3://$aws_account_id-snapdirector/snapdirector.py

