#ifdef __cplusplus 
extern "C" {
#endif
	#include <stdlib.h>
	#include <time.h>

	extern bool setupConn();	

	extern bool _setupConf();
	bool setupConf();
	extern bool _setupRun();
	bool setupRun();
	extern bool _tearDownRun();
	bool tearDownRun();
	extern bool _tearDownConf();
	bool tearDownConf();
	extern bool _predA(size_t, size_t, size_t);
	bool predA(size_t, size_t, size_t);
	extern bool _predP(size_t, size_t, size_t, size_t*);
	bool predP(size_t, size_t, size_t, size_t*);	
	
	extern size_t getNumReps();
	extern size_t getNumAgents();
	extern size_t getNumTicks();
	extern size_t getFragmentSize();
	extern const char* getFormula();
#ifdef __cplusplus
}
#endif
