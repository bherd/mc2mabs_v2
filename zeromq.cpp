/*
 * Copyright (c) 2016 Benjamin C. Herd.
 *
 * This file is part of MC2MABS.
 *
 * MC2MABS is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.

 * MC2MABS is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with MC2MABS. If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <iostream>
#include <vector>
#include <string>
#include <sstream>

#include "zmq.hpp"

#include "mc2mabs_c.h"

using namespace std;

int latestTick;

zmq::context_t context (1);
zmq::socket_t socket (context, ZMQ_REQ);
stringstream ss;
istringstream iss;
const string g_sTrue = "true";

bool send(string const& msg) {
	zmq::message_t request (msg.size());
   memcpy (request.data (), msg.c_str(), msg.size());
   //std::cout << "Sending " << msg << std::endl;
   socket.send (request);

   //  Get the reply.
   zmq::message_t reply;
   while(true) {
		try {
			socket.recv (&reply);
			string rpl = string(static_cast<char*>(reply.data()), reply.size());
   		//std::cout << "Received response: " << rpl << std::endl;
			return !rpl.compare(g_sTrue);
		}
		catch(zmq::error_t& e) {
			//std::cout << "W: interrupt received, proceeding…" << std::endl;
		}
	}
}

template<class T>
T sendS(string const& msg) {
	zmq::message_t request (msg.size());
   memcpy (request.data (), msg.c_str(), msg.size());
   //std::cout << "Sending " << msg << std::endl;
   socket.send (request);

   //  Get the reply.
   zmq::message_t reply;
   while(true) {
		try {
			socket.recv (&reply);
			string rpl = string(static_cast<char*>(reply.data()), reply.size());
			std::stringstream sstream(rpl);
			T res;
			sstream >> res;
			//std::cout << "Received response: " << rpl << std::endl;
			return res;
		}
		catch(zmq::error_t& e) {
			//std::cout << "W: interrupt received, proceeding…" << std::endl;
		}
	}
}

template<>
string sendS(string const& msg) {
	zmq::message_t request (msg.size());
   memcpy (request.data (), msg.c_str(), msg.size());
   //std::cout << "Sending " << msg << std::endl;
   socket.send (request);

   //  Get the reply.
   zmq::message_t reply;
   while(true) {
		try {
			socket.recv (&reply);
			string rpl = string(static_cast<char*>(reply.data()), reply.size());
			//std::cout << "Received response: " << rpl << std::endl;
			return rpl;
		}
		catch(zmq::error_t& e) {
			//std::cout << "W: interrupt received, proceeding…" << std::endl;
		}
	}
}

bool setupConn() {
	//  Prepare our context and socket    
   std::cout << "Connecting to simulation server" << std::endl;
   socket.connect ("tcp://localhost:5556");
}

bool setupConf() {
	return send("setupConf");
}

bool setupRun() {
	latestTick = -1;
	return send("setupRun");	
}

void step(size_t tick) {
	ss.str("");
	ss << "tick";
	send(ss.str());
}

bool tearDownConf() {
	bool res = send("tearDownConf");
	socket.close();
	return res;
}

bool tearDownRun() {
	return send("tearDownRun");
}


// ########## VERIFICATION INTERFACE ##########################

bool predP(
	size_t tick, 
	size_t predId, 
	size_t nAgents, 
	size_t* agentIds) {

	while(latestTick < (int)tick) {
		step(++latestTick);
	}

	ss.str("");
	ss << "predP:" << predId;
	bool res = send(ss.str());
	//cout << "Res: " << boolalpha << res << endl << flush;
	return res;
}

bool predA(
	size_t tick, 
	size_t predId, 
	size_t agentId) {
	
	while(latestTick < (int)tick) {
		step(++latestTick);
	}

	ss.str("");
	ss << "predA:" << predId << ":" << agentId;
	return send(ss.str());
}

size_t getNumReps() { size_t nReps = sendS<size_t>("numReps"); cout << "Num. replications: " << nReps << endl << flush; return nReps; }
size_t getNumAgents() { size_t nAgents = sendS<size_t>("numAgents"); cout << "Num. agents: " << nAgents << endl << flush; return nAgents; }
size_t getNumTicks() { size_t nTicks = sendS<size_t>("numTicks"); cout << "Num. ticks: " << nTicks << endl << flush; return nTicks; }
size_t getFragmentSize() { return getNumTicks(); }
const char* getFormula() { string formula = sendS<string>("formula"); cout << "Formula: " << formula << endl << flush; return formula.c_str(); } //{ return "FinallyS (PredS 0)"; }


