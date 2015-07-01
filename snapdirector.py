#!/usr/bin/python
import boto
import time
import sys
import glob
import os
import string
import subprocess
import os.path
import argparse
import logging
import ConfigParser

class NextAvailableDeviceFinder:
    """Instances of this class return the next available storage device, /dev/xvdX, where
    X is a lower case letter a-z (in reverse order, starting from z). """
    def __init__(self):
        self.available_letters = []

        for letter in string.lowercase:
            self.available_letters.append(letter)

        os.chdir("/dev")

        for existing_device in glob.glob("xvd?"):
            device_letter = existing_device[-1]
            self.available_letters.remove(device_letter)

    def find_next_device(self):
        return "/dev/xvd%s" % (self.available_letters.pop())

class Sleep:
    """Sleep with a 1.5x exponential backoff"""
    def __init__(self, seconds=3):
        self.seconds = seconds

    def sleep(self):
        logging.debug("Sleeping for %d seconds\n" % (int(self.seconds)))
        time.sleep(int(self.seconds))
        self.seconds *= 1.5

class OpendedupWrangler:
    def __init__(self, sdfs_volume_name):
        self.sdfs_volume_name = sdfs_volume_name
        config = ConfigParser.ConfigParser()
        config.read('/usr/local/etc/snapdirector.cfg')
        self.bucketname = config.get('general', 'bucketname')
        self.aws_access_key_id = config.get('general', 'aws_access_key_id')
        self.aws_secret_access_key = config.get('general', 'aws_secret_access_key')


    def ensure_sdfs_volume_exists(self):
        """This is supposed to be idempotent"""
        if not os.path.isfile("/etc/sdfs/%s-volume-cfg.xml" % (self.sdfs_volume_name)):
            logging.info("No /etc/sdfs/%s-volume-cfg.xml, attempting to sync from s3" % (self.sdfs_volume_name))
            command = "aws s3 sync s3://%s/etc/sdfs /etc/sdfs" % (self.bucketname)
            subprocess.check_call(command.split(" "))
            if not os.path.isfile("/etc/sdfs/%s-volume-cfg.xml" % (self.sdfs_volume_name)):
                logging.info("Still no /etc/sdfs/%s-volume-cfg.xml so it probably doesn't exist yet. Creating." % (self.sdfs_volume_name))
                subprocess.check_call(command.split(" "))
                command = "bash /sbin/mkfs.sdfs"
                command += " --volume-name=%s" % (self.sdfs_volume_name)
                command += " --volume-capacity=256TB"
                command += " --aws-enabled=true"
                command += " --cloud-access-key=%s" % (self.aws_access_key_id)
                command += " --cloud-bucket-name=%s" % (self.bucketname)
                command += " --cloud-secret-key=%s" % (self.aws_secret_access_key)
                command += " --chunk-store-encrypt=true"
                command += " --aws-bucket-location=US"
                subprocess.check_call(command.split(" "))
            else:
                logging.info("Looks like there's an sdfs on S3. Got /opt/sdfs already, going to also get /etc/sdfs and /var/log/sdfs")
                command = "aws s3 sync s3://%s/opt/sdfs /opt/sdfs" % (self.bucketname)
                subprocess.check_call(command.split(" "))
                command = "aws s3 sync s3://%s/var/log/sdfs /var/log/sdfs" % (self.bucketname)
                subprocess.check_call(command.split(" "))

    def start_sdfs(self):
        command = "bash /sbin/mount.sdfs %s /media/%s/" % (self.sdfs_volume_name, self.sdfs_volume_name)
        self.mount_process = subprocess.Popen(command.split(" "))
        time.sleep(10)

    def stop_and_sync_sdfs(self):
        command = "killall java"                       # a bit ham-handed
        try:
            subprocess.check_call(command.split(" "))
            time.sleep(30)
        except subprocess.CalledProcessError:
            logging.exception("killall java failed, which is probably just fine")

        command = "aws s3 sync /opt/sdfs s3://%s/opt/sdfs" % (self.bucketname)
        subprocess.check_call(command.split(" "))

        command = "aws s3 sync /etc/sdfs s3://%s/etc/sdfs" % (self.bucketname)
        subprocess.check_call(command.split(" "))

        command = "aws s3 sync /var/log/sdfs s3://%s/var/log/sdfs" % (self.bucketname)
        subprocess.check_call(command.split(" "))

def get_settings_dict_from_string(settings_string):
    settings = {}
    for setting in settings_string.split(","):
        [key, value] = setting.split("=")
        settings[key] = value

    return settings

class SnapDirector:
    def __init__(self, c, volume_id):
        self.c = c
        self.volume_id = volume_id

    def create_snapshot(self):
        self.volume = self.c.get_all_volumes(filters={'volume_id': self.volume_id})[0]
        skip = False
        if 'snapshot-director-settings' in self.volume.tags.keys():
            self.volume_settings = get_settings_dict_from_string(self.volume.tags['snapshot-director-settings'])
            if 'original-volume-id' in self.volume_settings.keys(): skip = True
            try:
                if self.volume_settings['skip-me'] == "True": skip = True
            except:
                pass
        if not skip:
            self.snapshot = self.c.create_snapshot(self.volume.id)
            logging.info("Creating snapshot for volume %s" % (self.volume.id))
        else:
            logging.info("Will not create snapshot for volume %s because it's not an original or has 'skip-me' turned on" % (self.volume.id))

        sleep = Sleep()
        while True:
            self.snapshot.update()
            if self.snapshot.status == 'completed':
                logging.info("Finished creating snapshot %s for volume %s" % (self.snapshot, self.snapshot.volume_id))
                return
            sleep.sleep()


    def create_volume(self, availability_zone):
        logging.info("Creating volume...")

        self.volume = self.snapshot.create_volume(zone=availability_zone, volume_type="io1", iops=self.snapshot.volume_size * 30)
        tag_value = "original-volume-id=%s,from-snapshot-id=%s,from-snapshot-start-time=%s" % (self.snapshot.volume_id, self.snapshot.id, self.snapshot.start_time)
        self.volume.add_tag('snapshot-director-settings', tag_value)
        self.volume.add_tag('original-volume-id', self.snapshot.volume_id)
        self.volume.add_tag('from-snapshot-id', self.snapshot.id)
        self.volume.add_tag('from-snapshot-start-time', self.snapshot.start_time)

        sleep = Sleep()
        while True:
            self.volume.update()
            if self.volume.status == 'available':
                self.volume_settings = get_settings_dict_from_string(self.volume.tags['snapshot-director-settings'])
                logging.info("Finished creating new volume %s based on volume %s's snapshot" % (self.volume.id, self.volume_settings['original-volume-id']))
                return
            sleep.sleep()

    def attach_volume(self, instance_id):
        next_available_device_finder = NextAvailableDeviceFinder()

        device = next_available_device_finder.find_next_device()
        a = self.c.attach_volume(self.volume.id, instance_id, device)

        sleep = Sleep()
        while True:
            self.volume.update()
            if self.volume.status == 'in-use':
                if os.path.exists(self.volume.attach_data.device):
                    volume_settings = get_settings_dict_from_string(self.volume.tags['snapshot-director-settings'])
                    logging.info("%s mounted on %s is a copy of %s from snapshot %s started at %s" % (
                            self.volume.id,
                            self.volume.attach_data.device,
                            self.volume_settings['original-volume-id'],
                            self.volume_settings['from-snapshot-id'],
                            self.volume_settings['from-snapshot-start-time']
                        )
                    )
                    return
            sleep.sleep()

    def add_volume_to_dedup_catalog(self):
        logging.info("Backing up volume")
        try:
            volume_settings = get_settings_dict_from_string(self.volume.tags['snapshot-director-settings'])
            logging.info("Going to try dd if=%s of=/media/s3backed0/%s__%s__%s" % (
                    self.volume.attach_data.device,
                    self.volume_settings['original-volume-id'],
                    self.volume_settings['from-snapshot-id'],
                    self.volume_settings['from-snapshot-start-time']
                )
            )
            subprocess.check_call(["ls", "-l", self.volume.attach_data.device])
            subprocess.check_call([
                "dd",
                "bs=128k",
                "if=%s" % (self.volume.attach_data.device),
                "of=/media/s3backed0/%s__%s__%s" % (volume_settings['original-volume-id'], volume_settings['from-snapshot-id'], volume_settings['from-snapshot-start-time'])
            ])
            logging.info("Backup complete for %s" % (volume_settings['original-volume-id']))
        except:
            logging.exception("Backup FAILED for %s" % (volume_settings['original-volume-id']))

    def detach_and_delete_volume(self):
        logging.info("Deleting snapshot")
        self.snapshot.delete()

        logging.info("Detaching volume")
        self.volume.detach()

        sleep = Sleep()
        while True:
            self.volume.update()
            if self.volume.status == 'available':
                logging.info("Volume detached")
                self.volume.delete()
                logging.info("Volume deleted")
                return
            sleep.sleep()

if __name__ == "__main__":
    pass

#touch "/mnt/`date`"
#time python snapdirector.py
#time dd if=/dev/xvdu of=/media/s3backed0/vol-7db9f696__snap-f800a492__2015-06-19_13:48:05
#python2.6 /usr/bin/s3cmd du s3://290093585298-snapdirector
#du -hs /opt/sdfs/volumes/s3backed0/*
#aws s3 ls s3://290093585298-snapdirector | wc -l

