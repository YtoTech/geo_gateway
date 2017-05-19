import socket
import logging
import logging.handlers
import sys

HOST = '0.0.0.0'
PORT = 2222
BUFFER_SIZE = 1024

# Logger (file + console)
rootLogger = logging.getLogger()
rootLogger.setLevel(logging.DEBUG)
# Rotating logging in a file.
rotatingFileHandler = logging.handlers.RotatingFileHandler(
	'log.txt', mode='a', maxBytes=500000, backupCount=3
)
rootLogger.addHandler(logging.StreamHandler(sys.stdout))
rootLogger.addHandler(rotatingFileHandler)
# TODO Do a data only log file.

# TODO To handle multiple connection, use select
# https://docs.python.org/3/library/select.html
# But better code that logic in Erlang, with a proper process model.
# Then just spam out messages from Erlang VM to datastores and applications,
# using a message broker with ACK -- so we sure the data as been received
# on the other part.

s = socket.socket()
logging.info('Binding to {}:{}'.format(HOST, PORT))
s.bind((HOST, PORT))
logging.debug('Listening for connections')
s.listen(5)
while True:
    logging.debug('Waiting for connection')
    conn, addr = s.accept()
    print(addr)
    logging.info('Connection accepted from {}'.format(addr))
    logging.debug('Beginning to receive... (buffer size is {})'.format(BUFFER_SIZE))
    receivedData = ''
    try:
        while True:
            partData = conn.recv(BUFFER_SIZE)
            logging.debug('Partial received data: {}'.format(partData))
            receivedData += partData
            if len(partData) < BUFFER_SIZE:
                logging.info('End of the stream, connection closed by peer {}'.format(addr))
                logging.info(receivedData)
                break
    except socket.error as e:
        logging.exception(e)
