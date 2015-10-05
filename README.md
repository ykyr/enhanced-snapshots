# Enhanced Snapshots
**[Sungard Availability Services](http://sungardas.com) | [Labs](http://blog.sungardas.com/CTOLabs/)**

![SGAS Logo](https://cloud.githubusercontent.com/assets/1557544/10001677/ed73c260-6070-11e5-86d1-8b85a146688d.png)
![Enhanced Snapshots Logo](https://cloud.githubusercontent.com/assets/1557544/10249820/431a4e82-68f5-11e5-9d97-95498364d9a4.png)

*Table of contents*
* [Product Description](#product-description)
* [Key Features](#key-features)
* [Quick start](#quick-start)
* [Getting Started](#getting-started)
* [Management Tasks](#management-tasks)
* [Removing the Enhanced Snapshots system](#removing-the-enhanced-snapshots-system)
* [IAM user creation (optional)](#iam-user-creation-optional)

# Product Description
The Sungard AS Enhanced Snapshots software manages backups for servers located in the Amazon Web Services cloud. The product will be useful for customers who want to reduce:
* the cost of storing backups
* time spent by IT engineers on routine backup management tasks.

Using an intuitive interface, you can easily automate routine tasks such as creation of backups and deletion of old backups. Since these tasks are automated, you will minimize the risks that are associated with human error.

Technical support is not available for the first version of the product; however, comments and suggestions are welcomed at sgaslabs+enhanced-snapshots@sungardas.com. Customer support service may be added in a future release.

Open source is another important feature of this solution and we plan to create a community that will support it. For the end user, Enhanced Snapshots will be free.

# Key Features
## Backup & Recovery 
* Performing backups of EBS volumes.
* The ability to perform recovery from historical backups.
* The ability to store backups of deleted volumes.
* Ability to quickly initiate recovery of backups.

## Schedule Policy
* Allows user to create scheduled tasks by minute, hour, day, week and month.
* Full support of CRON expressions.

## Retention Policy
* Allows user to automatically delete older backups based on the original volume’s size, count or age.
* A user can cancel any task before it is moved to the running state. 

## Management
* The ability to assign different (two) roles for users.
* Simple and intuitive wizard for initial setup process.

# Quick start
Or, stop making me read stuff and let me try it out!

We have provided an easy way to get up and running with Enhanced Snapshots via a [Condensation](https://github.com/SungardAS/condensation) [template](https://github.com/SungardAS/particles-enhanced-snapshots). To launch a CloudFormation stack based on the template, first decide which region you will deploy in. In that region you will need the following information:
* An [EC2 keypair](https://us-east-1.console.aws.amazon.com/ec2/v2/home?#KeyPairs)
* A [VPC id](https://console.aws.amazon.com/vpc/home?#vpcs:) (Not needed if you are using the Simple Stack option below.)
* An IP prefix from which inbound http/https/ssh connections will be allowed. If you're not sure, use your current [public IP address](http://www.myipaddress.com/show-my-ip-address/) with "/32" tacked on the end (like "1.2.3.4/32").

**Note** When creating the CloudFormation stack, on the Review page, make sure to check the box at the bottom of the page in the Capabilities section.

![Capabilities_checkbox](https://cloud.githubusercontent.com/assets/1557544/10256747/2feb29cc-6921-11e5-9d3b-d7974fb5753f.png)

Once you have collected that information, find your target region, note the AMI ID for that region, and click the corresponding Launch Stack link.

| Region         | AMI ID        | Simple Stack (default VPC) | VPC Stack (your VPC) |
| -------------- | ------------- | ------------ |---------- |
| eu-west-1      | ami-31714646  | [![Launch Stack](https://s3.amazonaws.com/cloudformation-examples/cloudformation-launch-stack.png)](https://console.aws.amazon.com/cloudformation/home?region=eu-west-1#/stacks/new?stackName=enhanced-snapshots&templateURL=http://particles-enhanced-snapshots.eu-west-1.s3.amazonaws.com/master/particles/cftemplates/enhanced_snapshots.template.json)|[![Launch Stack](https://s3.amazonaws.com/cloudformation-examples/cloudformation-launch-stack.png)](https://console.aws.amazon.com/cloudformation/home?region=eu-west-1#/stacks/new?stackName=enhanced-snapshots&templateURL=http://particles-enhanced-snapshots.eu-west-1.s3.amazonaws.com/master/particles/cftemplates/enhanced_snapshots_with_vpc.template.json)|
| us-east-1      | ami-25064440  | [![Launch Stack](https://s3.amazonaws.com/cloudformation-examples/cloudformation-launch-stack.png)](https://console.aws.amazon.com/cloudformation/home?region=us-east-1#/stacks/new?stackName=enhanced-snapshots&templateURL=http://particles-enhanced-snapshots.us-east-1.s3.amazonaws.com/master/particles/cftemplates/enhanced_snapshots.template.json)|[![Launch Stack](https://s3.amazonaws.com/cloudformation-examples/cloudformation-launch-stack.png)](https://console.aws.amazon.com/cloudformation/home?region=us-east-1#/stacks/new?stackName=enhanced-snapshots&templateURL=http://particles-enhanced-snapshots.us-east-1.s3.amazonaws.com/master/particles/cftemplates/enhanced_snapshots_with_vpc.template.json)|
| us-west-1      | ami-572deb13  | [![Launch Stack](https://s3.amazonaws.com/cloudformation-examples/cloudformation-launch-stack.png)](https://console.aws.amazon.com/cloudformation/home?region=us-west-1#/stacks/new?stackName=enhanced-snapshots&templateURL=http://particles-enhanced-snapshots.us-west-1.s3.amazonaws.com/master/particles/cftemplates/enhanced_snapshots.template.json)|[![Launch Stack](https://s3.amazonaws.com/cloudformation-examples/cloudformation-launch-stack.png)](https://console.aws.amazon.com/cloudformation/home?region=us-west-1#/stacks/new?stackName=enhanced-snapshots&templateURL=http://particles-enhanced-snapshots.us-west-1.s3.amazonaws.com/master/particles/cftemplates/enhanced_snapshots_with_vpc.template.json)|
| us-west-2      | ami-dc56b3ef  | [![Launch Stack](https://s3.amazonaws.com/cloudformation-examples/cloudformation-launch-stack.png)](https://console.aws.amazon.com/cloudformation/home?region=us-west-2#/stacks/new?stackName=enhanced-snapshots&templateURL=http://particles-enhanced-snapshots.us-west-2.s3.amazonaws.com/master/particles/cftemplates/enhanced_snapshots.template.json)|[![Launch Stack](https://s3.amazonaws.com/cloudformation-examples/cloudformation-launch-stack.png)](https://console.aws.amazon.com/cloudformation/home?region=us-west-2#/stacks/new?stackName=enhanced-snapshots&templateURL=http://particles-enhanced-snapshots.us-west-2.s3.amazonaws.com/master/particles/cftemplates/enhanced_snapshots_with_vpc.template.json)|

**Note** Due to feature differences between the AWS regions, the service is not yet available in the following regions: ap-northeast-1, ap-southeast-1, ap-southeast-2, eu-central-1, eu-west-1 and sa-east-1.

Once the CloudFormation stack has finished building, go to its Outputs tab at the bottom of the AWS Console and click the URL, then proceed to [Getting Started](#getting-started).

# Getting Started
**Note** If you have not followed the [Quick start](#quick-start) section above, then you will first need to manually [create an IAM user](#iam-user-creation-optional) and then create an EC2 instance using the Enhanced Snapshots AMI, which can be found in the first table above.

*Step 1*

For the first login please use the following credentials:
* Login: admin@enhancedsnapshots
* Password:  Your AWS EC2 Instance ID (available on the [AWS console](https://console.aws.amazon.com/ec2))
![Login](https://cloud.githubusercontent.com/assets/14750068/10096550/1bbf5294-637b-11e5-93d2-e1de26060c46.png)

*Step 2*

**Note**
This step is only required if you built your environment directly from our AMI rather than following the [Quick start](#quick-start) section above.

Enter the AWS Public Key and AWS Secret Key
![Settings](https://cloud.githubusercontent.com/assets/14750068/10096549/1bbe26a8-637b-11e5-9580-0b86c72f9839.png)

*Step 3*

The next picture shows the list of additional resources that will be created: S3 bucket, SDFS settings, SQS Queue, and DynamoDB tables. To view more information these resource the user can click the question mark.
![Settings(2)](https://cloud.githubusercontent.com/assets/14750068/10096552/1bc0a446-637b-11e5-993d-ec3ec9411c2b.png)

**Warning**
Once "Setup" has been clicked on this screen, these settings cannot be changed.

*Step 4*

Time to create a first user. The first user always will receive admin rights. Email is used as user ID.
![New user](https://cloud.githubusercontent.com/assets/14750068/10096551/1bc07480-637b-11e5-8bbb-e1720c1959ca.png)

*Step 5*

After the system is received all necessary input data it will create all necessary environment.
![Please wait](https://cloud.githubusercontent.com/assets/14750068/10096553/1bc35a1a-637b-11e5-8a1a-857cf14e010f.png)

After the configurations process will be successfully completed, the following notification will appear.
![Congratulations](https://cloud.githubusercontent.com/assets/14750068/10096554/1bd25362-637b-11e5-91c8-32d0dcee7498.png)

*Step 6*

The system automatically redirects the user to the login page. Now the user will use their credentials that were created in step 4.

![Login](https://cloud.githubusercontent.com/assets/14750068/10096556/1bdcd81e-637b-11e5-86ff-205b3d992a13.png)

After logging in, the list of EBS volumes for the local region is displayed.
![Volumes](https://cloud.githubusercontent.com/assets/14750068/10096555/1bda2dd0-637b-11e5-86b5-87d4463e337d.png)


# Management Tasks
## Peforming Manual Backups
To perform a backup, the user selects the appropriate EBS volume and clicks the button Backup selected. Also, the user can configure multi-backup, or several volumes during one task. For this, a user selects several appropriate volumes and clicks the button Backup selected. (Backups are performed one at a time.)
![Volumes (backup selected)](https://cloud.githubusercontent.com/assets/14750068/10023447/e410e4d8-615a-11e5-8bc3-93b9b9580528.png)

## Creating a Schedule
The user can automate the process of creating backups thanks to the Schedule feature. Schedules can be edited with Enhanced Snapshots web UI; schedules are displayed and stored in [Cron](https://en.wikipedia.org/wiki/Cron) format. The interval of backups is from one minute to one year. If necessary, schedules can be disabled.

![New schedule](https://cloud.githubusercontent.com/assets/14750068/10096546/1b936620-637b-11e5-859c-6e4e1b58d62e.png)

Users can also edit or delete existing schedules.
![Schedule menu](https://cloud.githubusercontent.com/assets/1557544/10000004/8c13719a-6067-11e5-9775-11e166d143d1.png)

## Managing Retention Policies
The retention policy function that allows the user to automatically delete backups according certain conditions: size limit, count limit days limit.
![Edit retention rule](https://cloud.githubusercontent.com/assets/1557544/9996404/c7337b3e-6054-11e5-8e12-5a0c7e139134.png)

Only one retention policy can be created for each volume. If a volume does not have any backups, the retention policy cannot be created.

Filters for users are available in order to sort according to different parameters: Volume ID, Name, Size, Instance ID and date of creation.
![Filter](https://cloud.githubusercontent.com/assets/14750068/10023443/e40fcada-615a-11e5-8412-5a090a3917a7.png)

##  Other Management Tasks
A list of all active and pending tasks can be found in the tab Tasks. The user can cancel any task before it starts running.

A list of all users is available in the Users tab. All information about users except their passwords is displayed. Users with admin rights can make changes to all users (even other administrators). A user without administrative rights can edit only their user profile. If there is only one administrator user, administrator rights cannot be revoked.

# Removing the Enhanced Snapshots system
If you choose to remove Enhanced Snapshots, you can do so by clicking the Uninstall button on the Settings tab.
![Settings](https://cloud.githubusercontent.com/assets/14750068/10096547/1baaef7a-637b-11e5-8a70-09a38198abfa.png)

The system will continue with the removal of all resources once you enter the EC2 Instance Id for the EC2 instance that Enhanced Snapshots is running on. 

![Delete](https://cloud.githubusercontent.com/assets/14750068/10096548/1bbace0e-637b-11e5-9e5e-a295bd8464c9.png)

The following resources are deleted:
* EC2 Instance 
* S3 bucket and all backup data
* SQS queue
* DynamoDB tables

**Note** It may take several minutes to delete all the resources, especially if backup data has been stored.

# IAM user creation (optional)
If you are creating an instance from the AMI directly without using the provided CloudFormation template, you must first create an IAM user with the following policy.
```
 {
    "Version": "2012-10-17", 
    "Statement": [
        {
            "Sid": "1",
            "Effect": "Allow",  
            "Action": [  
                "iam:GetUser",  
                "iam:ListRoles",  
                "ec2:AttachVolume",  
                "ec2:CreateSnapshot",  
                "ec2:CreateTags",  
                "ec2:CreateVolume",  
                "ec2:DeleteSnapshot",  
                "ec2:DeleteTags",  
                "ec2:DeleteVolume",  
                "ec2:DescribeAvailabilityZones",  
                "ec2:DescribeInstanceAttribute",  
                "ec2:DescribeInstanceStatus",  
                "ec2:DescribeInstances",  
                "ec2:TerminateInstances",  
                "ec2:DescribeRegions",  
                "ec2:DescribeReservedInstances",  
                "ec2:DescribeReservedInstancesListings",  
                "ec2:DescribeSnapshotAttribute",  
                "ec2:DescribeSnapshots",  
                "ec2:DescribeTags",  
                "ec2:DescribeVolumeAttribute",  
                "ec2:DescribeVolumeStatus",  
                "ec2:DescribeVolumes",  
                "ec2:DetachVolume",  
                "ec2:EnableVolumeIO",  
                "ec2:ModifyInstanceAttribute",  
                "ec2:ModifySnapshotAttribute",  
                "ec2:ModifyVolumeAttribute",  
                "ec2:ResetSnapshotAttribute",  
                "sqs:\*",  
                "s3:\*",  
                "dynamodb:\*"  
            ],  
            "Resource": "\*"
        }
    ]
}
```
Once the user is created, also create and save an API key, which will be needed to configure Enhanced Snapshots. 

Without a properly configured user, the following error message will appear during configuration:
![DynamoDBAccessDenied](https://cloud.githubusercontent.com/assets/14750068/10131876/08b816c8-65dc-11e5-871e-0f8d5fcdd303.png)
