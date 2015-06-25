REGION=`curl http://169.254.169.254/latest/dynamic/instance-identity/document|grep region|awk -F\" '{print $4}'`
datetime=`date "+%Y-%m-%d-%H-%m-%S"` ; aws cloudformation create-stack --region $REGION --stack-name "opendedup-$datetime" --template-url https://s3.amazonaws.com/290093585298.niktest/particles/cftemplates/traditional/opendedup.json --disable-rollback --capabilities CAPABILITY_IAM
