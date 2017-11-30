import smtplib
from email.mime.text import MIMEText
import sys

SERVER = 'localhost'
if len(sys.argv) > 1:
    SERVER = sys.argv[1]
PORT = 2525
if len(sys.argv) > 2:
    PORT = sys.argv[2]
USER = 'annon'
PASSWORD = 'coincoin'

# Create a text/plain message
msg = MIMEText(
	'''
	$GPLOC,A,1,200232.00,4717.67405,N,00001.31955,E,00000*0C
	$GPLOC,V,0,191809.00,,,,,00001*26
	$GPLOC,A,1,192307.00,4718.42826,N,00001.36627,W,00001*18
	$GPLOC,A,1,192504.00,4718.42208,N,00001.36296,W,00000*14
	$GPRMC,163734.00,A,4434.34454,N,00046.44022,E,0.015,0.00,230917,,,A*67
	$GPRMC,163734.00,A,4813.83336,N,00101.14936,E,0.015,0.00,230917,,,A*67
	'''
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
