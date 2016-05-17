#!/bin/bash
#Use this pattern to deploy ami into regions
# ./ami_deploy.sh source_ami_id source_ami_region

regions=(us-east-1 \ 	#set regions where you want to deploy
	 us-west-1 \
	 us-west-2 \
	 eu-west-1 \
	 eu-central-1	\
	 ap-northeast-1 \
	 ap-northeast-2 \
	 ap-southeast-1 \
	 ap-southeast-2 \
	 sa-east-1
)
name="SungardAS-AMI-Amazon-Linux-x86_64-HVM-Enhanced-Snapshots-2016.05.13"	#set name of the new AMI
ami_description="Enhanced Snapshots "						#set ami description
filename=latest.json								#set output file name

#Check for the "AMI ID" parameter
if [ -z $1 ]; then
	echo "No parameter. Set source AMI ID. Please, use following pattern:"
	echo $'\n ./ami_deploy.sh source_ami_id source_ami_region\n'
	exit 1
else
	echo "Source AMI ID = $1"
fi

#Check for the "AMI region" parameter
if [ -z $2 ]; then
        echo "No parameter. Set source AMI region. Please, use following pattern:"
	echo $'\n ./ami_deploy.sh source_ami_id source_ami_region\n'
	exit 1
else
#Check region for validaion in supported regions array
	for i in ${regions[@]}; do
	        if [ "$2" == "$i" ]
        	then
                	valid=1
	        fi
	done
#If source region wasn't found in "regions" array
	if [ "$valid" != "1" ]; then
		echo "-------------------------------------------------"
		echo "Cant find source region in \"Regions array\". Please check this region in script variable \"regions\". "
		echo "$2 region is invalid"
	        exit 1
	fi
        echo "Source AMI region = $2"
fi

#Info message
echo "--------------------------"
echo "{" >$filename
for i in ${regions[@]}; do
#Skip if source region
	if [ "$i" == "$2" ] #${regions[-1]}"
	then
        	continue
	fi
#Copy AMI, parse and output into file
	aws ec2 copy-image --source-image-id $1 --source-region $2 --region $i --name $name  --description "$ami_description" |grep ImageId |sed "s/ImageId/$i/g" |sed 's/:/:{"ami":/g' |sed 's/$/},/g' >>$filename
done

#Add source region and AMI id into file
echo "    \"$2\":{\"ami\": \"$1\"}" >>$filename
echo "}" >>$filename

#Result output
echo "Use \"cat $filename\" to get new ami IDs"
cat $filename
