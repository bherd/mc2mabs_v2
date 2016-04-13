import time
import zmq

context = zmq.Context()
socket = context.socket(zmq.REP)
socket.bind("tcp://*:5556")

def handleSetupMsg(msg):
	socket.send_string("true")

def handleTearDownMsg(msg):
	socket.send_string("true")

def handleTickMsg(msg):
	socket.send_string("true")

def handlePredPMsg(msg):
	socket.send_string("false")
	
def handlePredAMsg(msg):
	socket.send_string("false")

while True:
	#  Wait for next request from client
	message = socket.recv()
	#print("Received request: %s" % message)

	if message.startswith("setup"):
		handleSetupMsg(message)
		continue
	elif message.startswith("tearDown"):
		handleTearDownMsg(message)
		continue
	elif message.startswith("tick"):
		handleTickMsg(message)
		continue
	elif message.startswith("predA"):
		handlePredAMsg(message)
		continue
	elif message.startswith("predP"):
		handlePredPMsg(message)
		continue
	else:
		#  Send reply back to client
		socket.send_string("false")
