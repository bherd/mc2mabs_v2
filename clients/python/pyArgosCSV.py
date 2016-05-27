#
# Copyright (c) 2016 Benjamin C. Herd.
#
# This file is part of MC2MABS.
#
# MC2MABS is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# MC2MABS is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with MC2MABS. If not, see <http://www.gnu.org/licenses/>.

import time
import zmq
import sys
import os

currentRep = 0
currentTick = 0
traces = []

numAgents = 20
numReps = 100
numTicks = 1000

def handleSetupMsg(msg, socket):
	global currentTick
	global currentRep
	if msg == "setupConf":
		currentRep = 0
	elif msg == "setupRun":
		currentTick = 0
	socket.send_string("true")

def handleTearDownMsg(msg, socket):
	global currentRep
	if msg == "tearDownRun":
		currentRep += 1
	socket.send_string("true")

def handleTickMsg(msg, socket):
	global currentTick
	currentTick += 1
	socket.send_string("true")

def handlePredPMsg(msg, socket):
	idx = msg.split(":")[1]
	if idx == "0": # predicate 1: all robots are resting
		systemState = traces[currentRep][currentTick].strip(",\n").split(",")
		robotStates = [ robotState.strip("()\\n").split(";")[0] for robotState in systemState ]
		res = (len(robotStates) == numAgents)
		if res:
			socket.send_string("true")
		else:
			socket.send_string("false")

def handlePredAMsg(msg, socket):
	idx = msg.split(":")[1]
	agentId = int(msg.split(":")[2])
	if idx == "0": # check whether given robot is resting
		systemState = traces[currentRep][currentTick].strip(",\n").split(",")
		robotStates = [ robotState.strip("()\\n").split(";")[0] for robotState in systemState ]
		res = (robotStates[agentId] == 'H')
		if res:
			socket.send_string("true")
		else:
			socket.send_string("false")

def handleNumRepsMsg(msg, socket):
	socket.send_string("%d" % (numReps))

def handleNumAgentsMsg(msg, socket):
	socket.send_string("%d" % (numAgents))

def handleNumTicksMsg(msg, socket):
	socket.send_string("%d" % (numTicks))

def handleFormulaMsg(msg, socket):
	#f = "FinallyS (PredS 0)"
	f = "ForAgent (Just 1) (FinallyA (PredA 0))"
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
	files = [ "%s/%s" % (cwd,f) for f in filter(lambda f: f.startswith("foraging_"), os.listdir(cwd)) ]
	return files

def readTrace(file):
	f = open(file,'r')
	lines = f.readlines()
	return lines

def readTraces(files):
	return map(readTrace, files)

def main():
	global traces
	# Reading data files
	cwd = "/home/ben/repo/argos3-examples"
	files = readDataFiles(cwd)
	traces = readTraces(files)
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


