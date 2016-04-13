import time
import zmq
import sys

def handleSetupMsg(msg, socket):
	socket.send_string("true")

def handleTearDownMsg(msg, socket):
	socket.send_string("true")

def handleTickMsg(msg, socket):
	socket.send_string("true")

def handlePredPMsg(msg, socket):
	socket.send_string("false")
	
def handlePredAMsg(msg, socket):
	socket.send_string("false")

def setupConnection():
	context = zmq.Context()
	socket = context.socket(zmq.REP)
	socket.bind("tcp://*:5556")	
	return socket

def handleMessages(socket):
	while True:
		#  Wait for next request from client
		message = socket.recv()
		#print("Received request: %s" % message)

		if message.startswith("setup"):
			handleSetupMsg(message, socket)
			continue
		elif message.startswith("tearDown"):
			handleTearDownMsg(message, socket)
			continue
		elif message.startswith("tick"):
			handleTickMsg(message, socket)
			continue
		elif message.startswith("predA"):
			handlePredAMsg(message, socket)
			continue
		elif message.startswith("predP"):
			handlePredPMsg(message, socket)
			continue
		else:
			#  Send reply back to client
			socket.send_string("false")	

def main():
	# Setting up connection
	print "Setting up connection ...",
	sys.stdout.flush()
	socket = setupConnection()
	print "successful."
	print "Waiting for messages ...",
	sys.stdout.flush()
	handleMessages(socket)
	print "done."

if __name__ == "__main__":
    main()


