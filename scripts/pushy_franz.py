from kafka import message
from kafka import producer

EVENT_FILE = '/Users/lenards/devel/notes/km_events_examples/' + \
             'example_logs_tracking_km.txt'

TOPIC = 'EventStream'
HOST = 'localhost'
PORT = 9092

def rock_on(msg, producer):
    producer.send(message.Message(msg))


def rock_the_messages():
    with open(EVENT_FILE) as f:
        messages = f.readlines()

    p = producer.Producer(TOPIC, host=HOST, port=PORT)

    for msg in messages:
        rock_on(msg, p)


if __name__ == '__main__':
    rock_the_messages()