import boto
import time
import sys
import glob
import os
import string
import subprocess
import os.path
import argparse
import schedule
import boto3

def get_settings_dict_from_string(settings_string):
    settings = {}
    for setting in settings_string.split(","):
        [key, value] = setting.split("=")
        settings[key] = value

    return settings

def send_backup_request(volume_id):
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

                if 'backup-3x-daily' in volume_settings.keys() and volume_settings['backup-3x-daily'] == "True":
                    schedule.every().day.at("00:00").do(send_backup_request,{ "volume_id": volume.id })
                    schedule.every().day.at("08:00").do(send_backup_request,{ "volume_id": volume.id })
                    schedule.every().day.at("16:00").do(send_backup_request,{ "volume_id": volume.id })
                    sys.stderr.write("Backup scheduled every 8 hours for volume %s\n" % (volume.id))
                elif 'backup-daily' in volume_settings.keys() and volume_settings['backup-daily'] == "True":
                    schedule.every().day.at("00:00").do(send_backup_request,{ "volume_id": volume.id })
                    sys.stderr.write("Backup scheduled every 24 hours for volume %s\n" % (volume.id))

            else:
                sys.stderr.write("Will not create snapshot for volume %s because it's not an original or has 'skip-me' turned on\n" % (volume.id))

    def run(self):
        while True:
            schedule.run_pending()
            print "jobs:"
            for job in schedule.jobs:
                print job
            print "sleep; "
            time.sleep(1)

if __name__ == "__main__":
    print "hi"
    ss = SnapScheduler()
    ss.run()
    print "bye"

