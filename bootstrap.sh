#wget http://www.opendedup.org/downloads/SDFS-2.0.11-2.x86_64.rpm
#rpm --force -iv SDFS-2.0.11-2.x86_64.rpm
export PATH=$PATH:/sbin

aws_account_id=`curl -s http://169.254.169.254/latest/dynamic/instance-identity/document | grep accountId | cut -f2 -d: | cut -f2 -d\"`

mkdir /opt/sdfs
aws s3 sync s3://$S3BUCKETNAME/opt/sdfs /opt/sdfs
if [ -d /opt/sdfs/volumes/s3backed0 ] ; then
  echo "Existing sdfs found on S3"
  aws s3 sync s3://$S3BUCKETNAME/etc/sdfs /etc/sdfs
  aws s3 sync s3://$S3BUCKETNAME/var/log/sdfs /var/log/sdfs
else
  echo "No sdfs on S3, so create a local one"
  mkfs.sdfs\
    --volume-name=s3backed0 \
    --volume-capacity=256TB \
    --aws-enabled=true \
    --cloud-access-key=$AWS_ACCESS_KEY_ID \
    --cloud-bucket-name=$S3BUCKETNAME \
    --cloud-secret-key=$AWS_SECRET_ACCESS_KEY \
    --chunk-store-encrypt=true \
    --aws-bucket-location=US
fi

mkdir -p /media/s3backed0

configfile=/usr/local/etc/snapdirector.cfg
echo "[general]" >> $configfile
echo "bucketname = $S3S3BUCKETNAME" >> $configfile
echo "queuename = $QUEUENAME" >> $configfile
echo "sdfsvolumename = s3backed0" >> $configfile
echo "aws_region = $AWS_REGION" >> $configfile
echo "aws_access_key_id = $AWS_ACCESS_KEY_ID" >> $configfile
echo "aws_secret_access_key = $AWS_SECRET_ACCESS_KEY" >> $configfile

cp snapscheduler.py snapworker.py /usr/local/bin/
cp snapdirector.py /usr/local/lib/python2.7/site-packages/
cp snapdirector-init-script /etc/init.d/snapdirector
chkconfig --add snapdirector
chkconfig snapdirector on

/usr/local/bin/snapscheduler.py > /etc/cron.d/snapdirector

cat > /etc/cron.hourly/snapdirector <<EOF
#!/bin/sh

/usr/local/bin/snapscheduler.py > /etc/cron.d/snapdirector
EOF
chmod 755 /etc/cron.daily/snapdirector

cd /tmp
cat > awslogs-config-file <<EOF
general]
state_file = /var/awslogs/state/agent-state

[/var/log/snapworker.log]
file = /var/log/snapworker.log
log_group_name = /var/log/snapworker.log
log_stream_name = {instance_id}
datetime_format = %b %d %H:%M:%S
EOF

wget https://s3.amazonaws.com/aws-cloudwatch/downloads/latest/awslogs-agent-setup.py
chmod +x ./awslogs-agent-setup.py
./awslogs-agent-setup.py -n -r us-east-1 -c awslogs-config-file
