#!/usr/bin/python
import logging
logging.basicConfig(
    filename='/var/log/snapworker.log',
    level=logging.INFO,
    format='%(asctime)s - %(name)s %(levelname)s %(pathname)s:%(lineno)s(%(funcName)s) (pid%(process)d) - %(message)s'
)
import ConfigParser
import base64
import snapdirector
import boto
from snapdirector import OpendedupWrangler
import sys, traceback

import boto.sqs
from boto.sqs.message import Message

logging.info("snapworker.py starting")

config = ConfigParser.ConfigParser()
config.read('/usr/local/etc/snapdirector.cfg')
queuename = config.get('general', 'queuename')
sdfsvolumename = config.get('general', 'sdfsvolumename')
aws_region = config.get('general', 'aws_region')

conn = boto.sqs.connect_to_region(aws_region)
queue = conn.get_queue(queuename)

c = boto.connect_ec2()

my_instance_id = boto.utils.get_instance_metadata()['instance-id']
availability_zone = boto.utils.get_instance_metadata()['placement']['availability-zone']

message_count = 1
while True:
    for message in queue.get_messages(num_messages=1, wait_time_seconds=10):
        logging.info("Received message %d: %s" % (message_count, message.get_body()))
        volume_id = message.get_body()
        try:
            logging.info("Starting sdfs...")
            ow = OpendedupWrangler(sdfsvolumename)
            ow.ensure_sdfs_volume_exists()
            ow.start_sdfs()
        except:
            logging.error("Failed to start sdfs")
        try:
            sd = snapdirector.SnapDirector(c, volume_id)
            sd.create_snapshot()
            sd.create_volume(availability_zone)
            sd.attach_volume(my_instance_id)
            sd.add_volume_to_dedup_catalog()
            sd.detach_and_delete_volume()
            logging.info("Finished processing message %d" % (message_count))
        except:
            exc_type, exc_value, exc_traceback = sys.exc_info()
            logging.error("Failed to process message %d due to exception" % (message_count))
            for line in traceback.format_exc().splitlines():
                logging.error(line)

        ow.stop_and_sync_sdfs()

        queue.delete_message(message)
        message_count += 1
