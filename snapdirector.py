import boto
import time
import sys
import glob
import os
import string
import subprocess
import os.path
import argparse

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
        sys.stderr.write("Sleeping for %d seconds\n" % (int(self.seconds)))
        time.sleep(int(self.seconds))
        self.seconds *= 1.5

def get_settings_dict_from_string(settings_string):
    settings = {}
    for setting in settings_string.split(","):
        [key, value] = setting.split("=")
        settings[key] = value

    return settings

def create_snapshots(c, instance_id):
    sys.stderr.write("Creating snapshots for instance %s...\n" % (instance_id))
    pending_snapshots = []

    #for volume in c.get_all_volumes(filters={'attachment.instance-id': instance_id}):
    for volume in c.get_all_volumes(): #filters={'attachment.instance-id': instance_id}):
        skip = False
        if 'snapshot-director-settings' in volume.tags.keys():
            volume_settings = get_settings_dict_from_string(volume.tags['snapshot-director-settings'])
            if 'original-volume-id' in volume_settings.keys(): skip = True
            try:
                if volume_settings['skip-me'] == "True": skip = True
            except:
                pass
        if not skip:
            snapshot = c.create_snapshot(volume.id)
            pending_snapshots.append(snapshot)
            sys.stderr.write("Creating snapshot for volume %s\n" % (volume.id))
        else:
            sys.stderr.write("Will not create snapshot for volume %s because it's not an original or has 'skip-me' turned on\n" % (volume.id))

    new_snapshots = []

    sleep = Sleep()
    while len(pending_snapshots) > 0:
        sleep.sleep()
        for snapshot in pending_snapshots:
            snapshot.update()
            if snapshot.status == 'completed':
                sys.stderr.write("Finished creating snapshot %s for volume %s\n" % (snapshot, snapshot.volume_id))
                pending_snapshots.remove(snapshot)
                new_snapshots.append(snapshot)

    sys.stderr.write("All snapshots completed\n")

    return new_snapshots

def create_volumes(c, availability_zone, new_snapshots):
    sys.stderr.write("Creating volumes...\n")
    pending_volumes = []

    for snapshot in new_snapshots:
        volume = snapshot.create_volume(zone=availability_zone, volume_type="io1", iops=5000)
        tag_value = "original-volume-id=%s,from-snapshot-id=%s,from-snapshot-start-time=%s" % (snapshot.volume_id, snapshot.id, snapshot.start_time)
        volume.add_tag('snapshot-director-settings', tag_value)
        volume.add_tag('original-volume-id', snapshot.volume_id)
        volume.add_tag('from-snapshot-id', snapshot.id)
        volume.add_tag('from-snapshot-start-time', snapshot.start_time)
        pending_volumes.append(volume)

    new_volumes = []

    sleep = Sleep()
    while len(pending_volumes) > 0:
        sleep.sleep()
        for volume in pending_volumes:
            volume.update()
            if volume.status == 'available':
                volume_settings = get_settings_dict_from_string(volume.tags['snapshot-director-settings'])
                sys.stderr.write("Finished creating new volume %s based on volume %s's snapshot\n" % (volume.id, volume_settings['original-volume-id']))
                pending_volumes.remove(volume)
                new_volumes.append(volume)

    sys.stderr.write("All volumes created\n")

    return new_volumes

def attach_volumes(c, instance_id, new_volumes):
    next_available_device_finder = NextAvailableDeviceFinder()

    for volume in new_volumes:
        device = next_available_device_finder.find_next_device()
        a = c.attach_volume(volume.id, instance_id, device)

    volumes_being_attached = new_volumes

    attached_volumes = []

    sleep = Sleep()
    while len(volumes_being_attached) > 0:
        sleep.sleep()
        for volume in volumes_being_attached:
            volume.update()
            if volume.status == 'in-use':
                if os.path.exists(volume.attach_data.device):
                    volume_settings = get_settings_dict_from_string(volume.tags['snapshot-director-settings'])
                    sys.stderr.write("%s mounted on %s is a copy of %s from snapshot %s started at %s\n" % (
                            volume.id,
                            volume.attach_data.device,
                            volume_settings['original-volume-id'],
                            volume_settings['from-snapshot-id'],
                            volume_settings['from-snapshot-start-time']
                        )
                    )
                    volumes_being_attached.remove(volume)
                    attached_volumes.append(volume)

    sys.stderr.write("All volumes attached\n")

    return attached_volumes

def add_volumes_to_dedup_catalog(attached_volumes):
    sys.stderr.write("Backing up volumes\n")
    backed_up_volumes = []
    for volume in attached_volumes:
        try:
            volume_settings = get_settings_dict_from_string(volume.tags['snapshot-director-settings'])
            sys.stderr.write("Going to try dd if=%s of=/media/s3backed0/%s__%s__%s\n" % (
                    volume.attach_data.device,
                    volume_settings['original-volume-id'],
                    volume_settings['from-snapshot-id'],
                    volume_settings['from-snapshot-start-time']
                )
            )
            subprocess.check_call(["ls", "-l", volume.attach_data.device])
            subprocess.check_call([
                "dd",
                "bs=128k",
                "if=%s" % (volume.attach_data.device),
                "of=/media/s3backed0/%s__%s__%s" % (volume_settings['original-volume-id'], volume_settings['from-snapshot-id'], volume_settings['from-snapshot-start-time'])
            ])
            backed_up_volumes.append(volume)
            sys.stderr.write("Backup complete for %s\n" % (volume_settings['original-volume-id']))
        except:
            sys.stderr.write("Backup FAILED for %s\n" % (volume_settings['original-volume-id']))

    sys.stderr.write("Done backing up volumes\n")
    return backed_up_volumes

def detach_and_delete_volumes(volumes):
    sys.stderr.write("Detaching volumes\n")
    for volume in volumes:
        volume.detach()

    detached_volumes = []

    sleep = Sleep()
    while len(volumes) > 0:
        sleep.sleep()
        for volume in volumes:
            volume.update()
            if volume.status == 'available':
                volumes.remove(volume)
                detached_volumes.append(volume)

    sys.stderr.write("Deleting volumes\n")
    for volume in detached_volumes:
        volume.delete()

c = boto.connect_ec2()

my_instance_id = boto.utils.get_instance_metadata()['instance-id']

parser = argparse.ArgumentParser()

parser.add_argument(
    '--instance-id',
    dest='instance_id_to_back_up',
    action='store',
    default=my_instance_id,
    help='The id of the EC2 instance to back up (defaults to local machine - %s)' % (my_instance_id)
)

args = parser.parse_args()

availability_zone = boto.utils.get_instance_metadata()['placement']['availability-zone']

new_snapshots = create_snapshots(c, args.instance_id_to_back_up)

new_volumes = create_volumes(c, availability_zone, new_snapshots)

attached_volumes = attach_volumes(c, my_instance_id, new_volumes)

backed_up_volumes = add_volumes_to_dedup_catalog(attached_volumes)

detach_and_delete_volumes(backed_up_volumes)

#touch "/mnt/`date`"
#time python snapdirector.py
#time dd if=/dev/xvdu of=/media/s3backed0/vol-7db9f696__snap-f800a492__2015-06-19_13:48:05
#python2.6 /usr/bin/s3cmd du s3://290093585298-snapdirector
#du -hs /opt/sdfs/volumes/s3backed0/*
#aws s3 ls s3://290093585298-snapdirector | wc -l

