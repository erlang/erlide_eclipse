package org.erlide.jinterface;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlEventLoop implements Runnable {

	private IErlEventHandler fHandler;

	private Thread t;

	public ErlEventLoop(IErlEventHandler h) {
		fHandler = h;
	}

	public void run() {
		fHandler.init();
		boolean done = false;
		while (!fHandler.isTerminated() && !done) {
			OtpErlangObject msg;
			try {
				msg = fHandler.receiveEvent(fHandler.getTimeout());
				if (msg != null)
					fHandler.handleEvent(msg);
			} catch (final OtpErlangException e) {
				done = fHandler.exception(e);
			}

		}
	}

	public void start() {
		t = new Thread(this);
		t.setPriority(3); // low
		t.setDaemon(true);
		t.start();
	}

	public void stop() {
		t.interrupt();
	}

}
