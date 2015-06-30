import boto
import boto.sqs
import time
import sys
import glob
import os
import string
import subprocess
import os.path
import argparse
import ConfigParser
import schedule

def get_settings_dict_from_string(settings_string):
    settings = {}
    for setting in settings_string.split(","):
        [key, value] = setting.split("=")
        settings[key] = value

    return settings

def send_backup_request(aws_region, queuename, volume_id):
    print "Send request to back up", volume_id
    conn = boto.sqs.connect_to_region(aws_region)
    queue = conn.get_queue(queuename)

    from boto.sqs.message import Message

    m = Message()

    m.set_body(volume_id)

    queue.write(m)

def send_backup_request_old(volume_id):
    sqs = boto3.resource('sqs')
    try:
        queue = sqs.create_queue(QueueName='snapdirector-backup-queue')
    except:
        queue = sqs.get_queue_by_name(QueueName='snapdirector-backup-queue')

    response = queue.send_message(MessageBody=volume_id)

    print "backup volume", volume_id

class SnapScheduler:
    def __init__(self):
        self.c = boto.connect_ec2()
        sys.stderr.write("Find some stuff...\n")
        config = ConfigParser.ConfigParser()
        config.read('/usr/local/etc/snapdirector.cfg')
        queuename = config.get('general', 'queuename')
        aws_region = config.get('general', 'aws_region')


        for volume in self.c.get_all_volumes():
            skip = False
            volume_settings = {}
            if 'snapshot-director-settings' in volume.tags.keys():
                volume_settings = get_settings_dict_from_string(volume.tags['snapshot-director-settings'])
                if 'original-volume-id' in volume_settings.keys(): skip = True
                try:
                    if volume_settings['skip-me'] == "True": skip = True
                except:
                    pass
            if not skip:
                backup_arguments = {
                    "aws_region": aws_region,
                    "queuename": queuename,
                    "volume_id": volume.id
                }

                if 'backup-hours' in volume_settings.keys():
                    backup_hours =  volume_settings['backup-hours'].split(":")
                    for hour in backup_hours:
                        schedule.every().day.at(hour + ":22").do(send_backup_request, backup_arguments)
                        sys.stderr.write("Backup scheduled at %s:00 for volume %s\n" % (hour, volume.id))
            else:
                sys.stderr.write("Will not create snapshot for volume %s because it's not an original or has 'skip-me' turned on\n" % (volume.id))

        print "Currently scheduled jobs:"
        for job in schedule.jobs:
            print job

    def run(self):
        while True:
            schedule.run_pending()
#            print "sleep; "
            time.sleep(10)

class CronSnapScheduler:
    def __init__(self):
        self.c = boto.connect_ec2()
        sys.stderr.write("Find some stuff...\n")
        config = ConfigParser.ConfigParser()
        config.read('/usr/local/etc/snapdirector.cfg')
        queuename = config.get('general', 'queuename')
        aws_region = config.get('general', 'aws_region')
 
        conn = boto.sqs.connect_to_region(aws_region)
        queue_url = conn.get_queue(queuename).url

        for volume in self.c.get_all_volumes():
            skip = False
            volume_settings = {}
            if 'snapshot-director-settings' in volume.tags.keys():
                print "foo"
                volume_settings = get_settings_dict_from_string(volume.tags['snapshot-director-settings'])
                if 'original-volume-id' in volume_settings.keys(): skip = True
                try:
                    if volume_settings['skip-me'] == "True": skip = True
                except:
                    pass
            if not skip:
                backup_arguments = {
                    "aws_region": aws_region,
                    "queuename": queuename,
                    "volume_id": volume.id
                }

                if 'backup-hours' in volume_settings.keys():
                    backup_hours =  volume_settings['backup-hours'].split(":")
                    for hour in backup_hours:
                        #schedule.every().day.at(hour + ":22").do(send_backup_request, backup_arguments)
                        print "0 %s * * * root aws --region %s sqs send-message --queue-url %s --message-body %s" % (hour, aws_region, queue_url, volume.id)
                        sys.stderr.write("Backup scheduled at %s:00 for volume %s\n" % (hour, volume.id))
            else:
                sys.stderr.write("Will not create snapshot for volume %s because it's not an original or has 'skip-me' turned on\n" % (volume.id))


if __name__ == "__main__":
    #ss = SnapScheduler()
    ss = CronSnapScheduler()

