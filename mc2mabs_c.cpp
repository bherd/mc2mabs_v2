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

#include <iostream>
#include <map>
#include "mc2mabs_c.h"

using namespace std;

// agent predicate map
typedef map<size_t, bool> APredMap1;
typedef map<size_t, APredMap1> APredMap2;
typedef map<size_t, APredMap2> APredMap; 
APredMap m_predA;

// population predicate map
typedef map<size_t*, bool> PPredMap1;
typedef map<size_t, PPredMap1> PPredMap2;
typedef map<size_t, PPredMap2> PPredMap;
PPredMap m_predP;

bool _predA(
	size_t tick, 
	size_t predId, 
	size_t agentId) {
	
	// check if tick exists in agent predicate map
	APredMap::const_iterator it1 = m_predA.find(tick);
	if(it1 == m_predA.end()) {
		bool res = predA(tick, predId, agentId);
		m_predA[tick][predId][agentId] = res;
		return res;
	}	
	// check if predicate id exists in agent predicate map
	APredMap2::const_iterator it2 = it1->second.find(predId);
	if(it2 == it1->second.end()) {
		bool res = predA(tick, predId, agentId);
		m_predA[tick][predId][agentId] = res;
		return res;
	}
	//cout << "Tick " << tick << " exists." << endl << flush;
	return m_predA[tick][predId][agentId];
}

bool _predP(
	size_t tick, 
	size_t predId, 
	size_t nAgents, 
	size_t* agentIds) {
	
	// check if tick exists in population predicate map
	PPredMap::const_iterator it1 = m_predP.find(tick);
	if(it1 == m_predP.end()) {
		bool res = predP(tick, predId, nAgents, agentIds);
		m_predP[tick][predId][agentIds] = res;
		return res;
	}
	// check if predicate id exists in population predicate map
	PPredMap2::const_iterator it2 = it1->second.find(predId);
	if(it2 == it1->second.end()) {
		bool res = predP(tick, predId, nAgents, agentIds);
		m_predP[tick][predId][agentIds] = res;
		return res;
	}
	//cout << "Tick " << tick << " exists." << endl << flush;
	return m_predP[tick][predId][agentIds];
}

bool _setupConf() {
	srand(time(NULL));
	return setupConf();
}

bool _setupRun() {
	return setupRun();
}

bool _tearDownRun() {
	m_predA.clear();
	m_predP.clear();
	return tearDownRun();
}

bool _tearDownConf() {
	return tearDownConf();
}
