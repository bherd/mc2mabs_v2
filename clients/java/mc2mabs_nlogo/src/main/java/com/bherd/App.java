package com.bherd;
import org.zeromq.ZMQ;

/**
 * Hello world!
 *
 */
public class App 
{
		protected NetLogoConn nlc = new NetLogoConn();

		public App() {}

		protected void handleSetupMsg(String message, ZMQ.Socket socket) {
			//System.out.println("setup");
			String res = "false";
			if(message.equals("setupConf"))
				res = Boolean.toString(nlc.setupConf());
			else if(message.equals("setupRun"))
				res = Boolean.toString(nlc.setupRun());
			socket.send(res.getBytes(), 0);
		}
		
    protected void handleTearDownMsg(String message, ZMQ.Socket socket) {
			String res = "false";
			if(message.equals("tearDownConf"))
				res = Boolean.toString(nlc.tearDownConf());
			else if(message.equals("tearDownRun"))
				res = Boolean.toString(nlc.tearDownRun());
			socket.send("true".getBytes(), 0);
		}
		
		protected void handlePredAMsg(String message, ZMQ.Socket socket) {
			String res = Boolean.toString(nlc.predA(message.split(":")[1]));
			socket.send(res.getBytes(), 0);
		}

		protected void handlePredPMsg(String message, ZMQ.Socket socket) {
			String res = Boolean.toString(nlc.predP(message.split(":")[1]));
			socket.send(res.getBytes(), 0);
		}

		protected void handleTickMsg(String message, ZMQ.Socket socket) {
			//System.out.println("tick");
			String res = Boolean.toString(nlc.step());
			socket.send(res.getBytes(), 0);
		}

		public static void main( String[] args )
    {
				App a = new App();

        //  Prepare our context and publisher
        ZMQ.Context context = ZMQ.context(1);

        ZMQ.Socket socket = context.socket(ZMQ.REP);
        socket.bind("tcp://*:5556");

				while (!Thread.currentThread().isInterrupted()) {
            // Wait for next request from the client
            String message = socket.recvStr(0).trim();

						if(message.startsWith("setup"))
							a.handleSetupMsg(message, socket);
						else if(message.startsWith("tearDown"))
							a.handleTearDownMsg(message, socket);
						else if(message.startsWith("predA"))
							a.handlePredAMsg(message, socket);
						else if(message.startsWith("predP"))
							a.handlePredPMsg(message, socket);
						else if(message.startsWith("tick"))
							a.handleTickMsg(message, socket);
						else
							System.out.println("ERROR: Unknown message: " + message);
        }
        socket.close();
        context.term();
    }
}
