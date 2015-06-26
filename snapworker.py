#!/usr/bin/python
import boto3
import ConfigParser

config = ConfigParser.ConfigParser()
config.read('/usr/local/etc/snapdirector.cfg')
queuename = config.get('general', 'queuename')
aws_region = config.get('general', 'aws_region')

aws_session = boto3.session.Session(region_name=aws_region)

sqs = aws_session.resource('sqs')

queue = sqs.get_queue_by_name(QueueName=queuename)

message_count = 1
while True:
    for message in queue.receive_messages():
        print "Received message %d: %s" % (message_count, message)
        message_count += 1
    print "(Sleep...)"
    sleep(10)

