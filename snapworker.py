#!/usr/bin/python
import ConfigParser
import logging
import base64

import boto.sqs
from boto.sqs.message import Message

logging.basicConfig(filename='/var/log/snapworker.log',level=logging.INFO)

config = ConfigParser.ConfigParser()
config.read('/usr/local/etc/snapdirector.cfg')
queuename = config.get('general', 'queuename')
aws_region = config.get('general', 'aws_region')

conn = boto.sqs.connect_to_region(aws_region)
queue = conn.get_queue(queuename)

logging.info(str(dir(queue)))

message_count = 1
while True:
    for message in queue.get_messages(num_messages=1, wait_time_seconds=10):
        logging.info("Received message %d: %s" % (message_count, message.get_body()))
        queue.delete_message(message)
        message_count += 1
