import time
import zmq
import sys
import os

currentIndex = 0
files = []

def handleSetupMsg(msg, socket):
	socket.send_string("true")

def handleTearDownMsg(msg, socket):
	socket.send_string("true")

def handleTickMsg(msg, socket):
	global currentIndex
	currentIndex += 1
	socket.send_string("true")

def handlePredPMsg(msg, socket):
	idx = msg.split(":")[1]
	if idx == "0": # check whether robot is resting
		state = files[currentIndex]
		res = state.split(";")[1] == "R"
		print "Result: %b" % (b)
		socket.send_string("%b" % res)
	socket.send_string("false")	

def handlePredAMsg(msg, socket):
	socket.send_string("false")

def handleNumRepsMsg(msg, socket):
	socket.send_string("%d" % (10))

def handleNumAgentsMsg(msg, socket):
	socket.send_string("%d" % (10))

def handleNumTicksMsg(msg, socket):
	socket.send_string("%d" % (10))

def handleFormulaMsg(msg, socket):
	f = "FinallyS (PredS 0)"
	socket.send_string(f)

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
		elif message.startswith("numReps"):
			handleNumRepsMsg(message, socket)
			continue
		elif message.startswith("numAgents"):
			handleNumAgentsMsg(message, socket)
			continue
		elif message.startswith("numTicks"):
			handleNumTicksMsg(message, socket)
			continue
		elif message.startswith("formula"):
			handleFormulaMsg(message, socket)
			continue
		else:
			#  Send reply back to client
			socket.send_string("false")	

def readDataFiles(cwd):
	files = filter(lambda f: f.startswith("foraging_"), os.listdir(cwd))
	return files

def main():
	global files
	# Reading data files
	cwd = "/home/bherd/repo/argos3-examples"
	files = readDataFiles(cwd)
	print "%d files found." % len(files)
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


