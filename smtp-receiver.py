from datetime import datetime
import asyncore
from smtpd import SMTPServer
import os

# Code from https://stackoverflow.com/questions/2690965/a-simple-smtp-server

# TODO Make Python3 compatible. (format {} instead of %, print, etc.)
# Update it on SO https://stackoverflow.com/questions/2690965/a-simple-smtp-server
# TODO Use https://github.com/aio-libs/aiosmtpd/blob/master/examples/server.py
# to include user
# https://speakerdeck.com/pycon2017/barry-warsaw-aiosmtpd-a-better-asyncio-based-smtp-server
# See https://stackoverflow.com/questions/1138425/add-smtp-auth-support-to-python-smtpd-library-cant-override-the-method
# and https://stackoverflow.com/questions/44028565/aiosmtpd-python-smtp-server

DIRECTORY = 'tmp/'

class SmtpEmlDumperServer(SMTPServer):
    no = 0
    def process_message(self, peer, mailfrom, rcpttos, data):
        if not os.path.exists(DIRECTORY):
            os.makedirs(DIRECTORY)
        print(peer)
        print(mailfrom)
        print(data)
        filename = DIRECTORY + '{}-{}.eml'.format(
            datetime.now().strftime('%Y%m%d%H%M%S'),
            self.no
        )
        f = open(filename, 'w')
        f.write(data)
        f.close
        print('{} saved.'.format(filename))
        self.no += 1


def run():
    print('Starting server')
    foo = SmtpEmlDumperServer(('localhost', 25), None)
    try:
        # TODO Use asyncio
        # https://docs.python.org/3/library/asyncio-eventloop.html#asyncio-event-loop
        asyncore.loop()
    except KeyboardInterrupt:
        pass


if __name__ == '__main__':
	run()
