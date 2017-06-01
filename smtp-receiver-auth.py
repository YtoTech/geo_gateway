# https://pypi.python.org/pypi/secure-smtpd/3.0.0
# virtualenv venv
# TODO Missing dependency six
# https://github.com/bcoe/secure-smtpd/blob/master/setup.py#L19
# venv/bin/pip install six
# venv/bin/pip install secure-smtpd
# venv/bin/python smtp-receiver-auth.py
from secure_smtpd import SMTPServer
from datetime import datetime
import asyncore
import os
import logging
import secure_smtpd

# Update the package
# https://github.com/bcoe/secure-smtpd/issues
# TODO Create a complete Hello World
# + add installation instructions
# https://github.com/bcoe/secure-smtpd/issues/30

DIRECTORY = 'tmp/'
USERS = {
    'aaz-performance-1': 'coincoin'
}

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

class CredentialValidator:
    def validate(self, username, password):
        logger = logging.getLogger( secure_smtpd.LOG_NAME )
        if username in USERS and USERS[username] == password:
            return True
        return False


def run():
    print('Starting server')
    server = SmtpEmlDumperServer(
        ('localhost', 25),
        None,
        require_authentication=True,
        ssl=False,
        credential_validator=CredentialValidator()
    )
    try:
        server.run()
    except KeyboardInterrupt:
        pass


if __name__ == '__main__':
	run()
