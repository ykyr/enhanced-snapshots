#!/usr/bin/python
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

def get_settings_dict_from_string(settings_string):
    settings = {}
    for setting in settings_string.split(","):
        [key, value] = setting.split("=")
        settings[key] = value

    return settings

class CronSnapScheduler:
    def __init__(self):
        self.c = boto.connect_ec2()
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
                        # crontab cheat sheet: <minute> <hour> <day of month> <month> <day of week> <user> <command ...>
                        print "0 %s * * * root aws --region %s sqs send-message --queue-url %s --message-body %s" % (hour, aws_region, queue_url, volume.id)
                else:
                    print "0 0 * * * root aws --region %s sqs send-message --queue-url %s --message-body %s" % (aws_region, queue_url, volume.id)
            else:
                pass


if __name__ == "__main__":
    ss = CronSnapScheduler()

