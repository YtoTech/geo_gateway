import smtplib
from email.mime.text import MIMEText
import sys

# host = sys.argv[1]
# port = int(sys.argv[2])
SERVER = 'localhost'
PORT = 25
USER = 'aaz-performance-1'
PASSWORD = 'coincoin'

# Create a text/plain message
msg = MIMEText(
    '$GPLOC,358683066123549,A,1,093255.00,4834.37069,N,00105.96113,W,00000,3194000000003827*2A(35)'
)

expeditor = 'test@ytotech.com'
recipient = 'genloc@ytotech.com'
msg['Subject'] = 'A trame'
msg['From'] = expeditor
msg['To'] = 'genloc@ytotech.com'

print('Connecting to {}:{}'.format(SERVER, PORT))
server = smtplib.SMTP(SERVER, PORT)
print('Login as {}:{}'.format(USER, PASSWORD))
server.login(USER, PASSWORD)
print('Sending test packet...')
server.sendmail(expeditor, [recipient], msg.as_string())
print('Sent')
print('Close connection')
server.quit()
