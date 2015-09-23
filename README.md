# Enhanced Snapshots
**[Sungard Availability Services](http://sungardas.com) | [Labs](http://blog.sungardas.com/CTOLabs/)**

![SGAS Logo](https://cloud.githubusercontent.com/assets/1557544/10001677/ed73c260-6070-11e5-86d1-8b85a146688d.png)

*Table of contents*
* [Product Description](#product-description)
* [Key Features](#key-features)
* [Getting Started](#getting-started)
* [Management Tasks](#management-tasks)
* [Removing the Enhanced Snapshots system](#removing-the-enhanced-snapshots-system)

# Product Description
The Sungard AS Enhanced Snapshots tool is intended for managing backups for infrastructure that is located in the Amazon Web Services cloud. The product will be useful for customers who want to reduce:
* the cost of storing backups
* time spent by IT engineers on routine backup management tasks.

Using to an intuitive interface, you can automate routine tasks such as creation of backups and deletion of old (unused) backups easily. Since these tasks are automated, you will minimize the risks that are associated with human error.

Technical support is not available for the first version of the product. Comments and suggestions can be sent to AWSEnhancedSnapshots@sungardas.com. Customer support service may be added in a future release.

Open source is another important feature of this solution and we plan to create a community that will support it. For the end user, ASM will be free.

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

# Getting Started
*Step 1*

For the first login please use the following credentials:
* Login: admin@snapdirector
* Password:  Your AWS EC2 Instance ID (available on the [AWS console](https://console.aws.amazon.com/ec2))
![Login](https://cloud.githubusercontent.com/assets/1557544/9996392/c7223fe0-6054-11e5-8f26-d81fd5240840.png)

*Step 2*

Enter the AWS Public Key and AWS Secret Key
![Settings](https://cloud.githubusercontent.com/assets/1557544/9996391/c7212b5a-6054-11e5-9597-4e0e55e20343.png)

*Step 3*

The next picture shows the list of additional resources that will be created: S3 bucket, SDFS settings, SQS Queue, and DynamoDB tables. To view more information these resource the user can click the question mark.
![Settings(2)](https://cloud.githubusercontent.com/assets/14750068/10023444/e40ff136-615a-11e5-9f38-a3e153826ab8.png)

**Warning**
Once "Setup" has been clicked on this screen, these settings cannot be changed.

*Step 4*

Time to create a first user. The first user always will receive admin rights. Email is used as user ID.
![New user](https://cloud.githubusercontent.com/assets/1557544/9996394/c726a210-6054-11e5-968a-f3c989ffd358.png)

*Step 5*

After the system is received all necessary input data it will create all necessary environment.
![Please wait](https://cloud.githubusercontent.com/assets/14750068/10023445/e410406e-615a-11e5-9ae4-53e9b20a5b95.png)

After the configurations process will be successfully completed, the following notification will appear.
![Congratulations](https://cloud.githubusercontent.com/assets/14750068/10023442/e40e7c02-615a-11e5-8d36-46ae12bc50c8.png)

*Step 6*

The system automatically redirects the user to the login page. Now the user will use their credentials that were created during
wizard to log in.
![Login](https://cloud.githubusercontent.com/assets/1557544/9996396/c7270c32-6054-11e5-95ff-47632ba834b8.png)

After logging in, the list of EBS volumes for the local region is displayed.
![Volumes](https://cloud.githubusercontent.com/assets/14750068/10023446/e4102a70-615a-11e5-9eda-915d04bbb5b1.png)


# Management Tasks
## Peforming Manual Backups
To perform a backup, the user selects the appropriate EBS volume and clicks the button Backup selected. Also, the user can configure multi-backup, or several volumes during one task. For this, a user selects several appropriate volumes and clicks the button Backup selected. (Backups are performed one at a time.)
![Volumes (backup selected)](https://cloud.githubusercontent.com/assets/14750068/10023447/e410e4d8-615a-11e5-8bc3-93b9b9580528.png)

## Creating a Schedule
The user can automate the process of creating backups thanks to the Schedule feature. Schedules can be edited with Enhanced Snapshots web UI; schedules are displayed and stored in [Cron](https://en.wikipedia.org/wiki/Cron) format. The interval of backups is from one minute to one year. If necessary, schedules can be disabled.

![New schedule](https://cloud.githubusercontent.com/assets/1557544/9999927/1331a7ec-6067-11e5-868a-cf2a7c831168.png)

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
![Settings](https://cloud.githubusercontent.com/assets/1557544/10001465/cfdb5a70-606f-11e5-8e43-2334c7f14600.png)

The system will continue with the removal of all resources once you enter the EC2 Instance Id for the EC2 instance that Enhanced Snapshots is running on.

![Delete](https://cloud.githubusercontent.com/assets/1557544/10001468/d462832a-606f-11e5-8ec9-539f31c3d1db.png)

The following resources are deleted:
* EC2 Instance 
* S3 bucket and all backup data
* SQS queue
* DynamoDB tables
