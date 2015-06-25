aws_account_id=`curl -s http://169.254.169.254/latest/dynamic/instance-identity/document | grep accountId | cut -f2 -d: | cut -f2 -d\"`

echo ===snapdirector===
time python snapdirector.py
echo -n s3 object count:
aws s3 ls s3://$aws_account_id-snapdirector | wc -l
echo -n s3 size:
python2.6 /usr/bin/s3cmd du s3://$aws_account_id-snapdirector
echo -n catalog size:
du -hs /media/s3backed0/
echo ---volume info---
sdfscli --volume-info

aws s3 sync /opt/sdfs s3://$aws_account_id-snapdirector/opt/sdfs
aws s3 sync /etc/sdfs s3://$aws_account_id-snapdirector/etc/sdfs
aws s3 sync /var/log/sdfs s3://$aws_account_id-snapdirector/var/log/sdfs
