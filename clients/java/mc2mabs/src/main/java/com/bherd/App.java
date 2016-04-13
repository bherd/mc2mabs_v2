package com.bherd;

import java.util.Random;
import org.zeromq.ZMQ;

public class App {

    public static void main (String[] args) throws Exception {
        //  Prepare our context and publisher
        ZMQ.Context context = ZMQ.context(1);

        ZMQ.Socket socket = context.socket(ZMQ.REP);
        socket.bind("tcp://*:5556");

				while (!Thread.currentThread().isInterrupted()) {
            // Wait for next request from the client
            String string = socket.recvStr(0).trim();
            System.out.println("Received "+string);

            // Send reply back to client
            String reply = "true";
            socket.send(reply.getBytes(), 0);
        }
        socket.close();
        context.term();

	}
}

