package com.bherd;

import org.nlogo.headless.HeadlessWorkspace;
import org.nlogo.api.AgentSet;


/**
 * Hello world!
 *
 */
public class NetLogoConn 
{
		protected HeadlessWorkspace workspace;

		public NetLogoConn() {
		}

		public boolean setupConf() {
			try {
				String path = System.getenv("HOME") + "/tools/netlogo-5.2.1/";
				//String model = "/models/Sample Models/Earth Science/Fire.nlogo";
				String model = "/models/Sample Models/Networks/Virus on a Network.nlogo";
				workspace = HeadlessWorkspace.newInstance();						
				workspace.open(path+model);
				return true;
			}
			catch(Exception ex) {
				ex.printStackTrace();
				return false;
			}
		}

		public boolean setupRun() {
			try {
				workspace.command("set number-of-nodes 50");
				workspace.command("set virus-spread-chance 10");
				workspace.command("set recovery-chance 0");
	      //workspace.command("random-seed 0");
	      workspace.command("setup");	
				return true;
			}
			catch(Exception ex) {
				ex.printStackTrace();
				return false;
			}
		}

		public boolean step() {
			try {
				workspace.command("go");
				return true;
			}
			catch(Exception ex) {
				ex.printStackTrace();
				return false;
			}
		}

		public boolean predA(String predId) {
			return true;
		}

		public boolean predP(String predId) {
			try {
				double infTurtles = ((Double)workspace.report("count turtles with [infected?]")).doubleValue();
				double nTurtles = ((Double)workspace.report("count turtles")).doubleValue();
				return (infTurtles==nTurtles);
			}
			catch(Exception ex) {
				ex.printStackTrace();
				return false;
			}
		}

		public boolean tearDownRun() {
			return true;
		}

		public boolean tearDownConf() {
			try {				
				workspace.dispose();
				return true;
			} 
			catch(Exception ex) {
				ex.printStackTrace();
				return false;
			}
		}

    public static void main( String[] args )
    {
        try {
        }
        catch(Exception ex) {
            ex.printStackTrace();
        };
    }
}

