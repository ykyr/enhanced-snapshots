# Overview
Snapdirector is a system that stores AWS EC2 snapshot data in S3, with
compression and de-duplication provided by [Opendedup
SDFS](https://github.com/opendedup/sdfs).

# Description

The initial python implementation has two parts.

## Scheduler
The scheduler is a simple Python script that is run every minute by
cron. By looking at the contents of the snapshot-director-settings tag on
EC2 volumes, it determines which volumes should be backed up and at what
frequency. It outputs a secondary crontab which is written into
/etc/cron.d. The secondary crontab sends a message to an SQS queue at
the appropriate time for each protected volume.

## Worker
The worker, also Python, listens to an SQS queue for volumes to be
backed up. When it receives a message for a volume to back up, it does
the following:

1. Creates (if necessary) an S3-backed SDFS filesystem and mounts it
1. Creates a snapshot for the volume
1. Creates a temporary volume from the snapshot
1. Attaches the temporary volume
1. Copies the volume data to SDFS
1. Detaches the temporary volumes, deletes it, and deletes the snapshot
1. Unmounts SDFS and syncronizes its metadata to S3
