#wget http://www.opendedup.org/downloads/SDFS-2.0.11-2.x86_64.rpm
#rpm --force -iv SDFS-2.0.11-2.x86_64.rpm
export PATH=$PATH:/sbin

aws configure set region $AWS_REGION

aws_account_id=`curl -s http://169.254.169.254/latest/dynamic/instance-identity/document | grep accountId | cut -f2 -d: | cut -f2 -d\"`

mkdir /opt/sdfs
mkdir -p /media/s3backed0

configfile=/usr/local/etc/snapdirector.cfg
echo "[general]" >> $configfile
echo "bucketname = $S3BUCKETNAME" >> $configfile
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

cat > /etc/cron.d/snapscheduler <<EOF
* * * * * root /usr/local/bin/snapscheduler.py > /etc/cron.d/snapdirector
EOF

cd /tmp
cat > awslogs-config-file <<EOF
[general]
state_file = /var/awslogs/state/agent-state

[/var/log/snapworker.log]
file = /var/log/snapworker.log
log_group_name = /var/log/snapworker.log
log_stream_name = {instance_id}
datetime_format = %b %d %H:%M:%S
EOF

wget https://s3.amazonaws.com/aws-cloudwatch/downloads/latest/awslogs-agent-setup.py
chmod +x ./awslogs-agent-setup.py
./awslogs-agent-setup.py -n -r $AWS_REGION -c awslogs-config-file
chkconfig awslogs on
