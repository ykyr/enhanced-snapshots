import boto.utils
import boto.sqs

region = boto.utils.get_instance_metadata()['placement']['availability-zone'][:-1]
conn = boto.sqs.connect_to_region(region)
q = conn.create_queue('myqueue')

while True:
    m = q.read(wait_time_seconds=10)
    if m != None:
        print "Received message", m.get_body()
        q.delete_message(m)

