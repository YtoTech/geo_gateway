import socket
import sys

# HOST = 'localhost'
# PORT = 2222
host = sys.argv[1]
port = int(sys.argv[2])
BUFFER_SIZE = 1024

s = socket.socket()
print('Connecting to {}:{}'.format(host, port))
s.connect((host, port))
print('Sending test packet...')
s.send('$GPLOC,358683066123549,A,1,093255.00,4834.37069,N,00105.96113,W,00000,3194000000003827*2A(35)')
print('Sent')
print('Close connection')
s.close()
