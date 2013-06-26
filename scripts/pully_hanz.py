import re
import requests
from kafka import message
from kafka import consumer

# defaults
HOST = 'localhost'
PORT = 9092
TOPIC = 'EventStream'

TARGET = 'http://127.0.0.1:8000/'
SRV_URL = 'event/{0}/{1}'

KEY_REX = re.compile('\_k=([^(&|\W)]*)')
PROD_REX = re.compile('\_p=([^(&|\W)]*)')

def do_it(bucket, key, payload):
    headers = {'Content-Type': 'text/plain'}
    service = SRV_URL.format(bucket, key)
    url = TARGET + service
    req = requests.post(url, data=payload)
    print req.text


def process_and_post(msg):
    match_key = re.search(KEY_REX, str(msg.payload))
    match_prod = re.search(PROD_REX, str(msg.payload))
    if (match_prod is not None and
        match_key is not None):
        key = match_key.group(1)
        prod = match_prod.group(1)
        do_it(prod, key, msg.payload)


def main():
    c = consumer.Consumer(TOPIC, host=HOST, port=PORT)

    for msg in c.loop():
        process_and_post(msg)
        print msg

if __name__ == '__main__':
    main()