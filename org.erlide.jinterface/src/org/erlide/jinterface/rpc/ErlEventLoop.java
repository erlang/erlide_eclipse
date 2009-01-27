package org.erlide.jinterface.rpc;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlEventLoop implements Runnable {

	private IErlEventHandler fHandler;

	private Thread t;

	public ErlEventLoop(IErlEventHandler h) {
		this.fHandler = h;
	}

	public void run() {
		this.fHandler.init();
		boolean done = false;
		while (!this.fHandler.isTerminated() && !done) {
			OtpErlangObject msg;
			try {
				msg = this.fHandler.receiveEvent(this.fHandler.getTimeout());
				if (msg != null) {
					this.fHandler.handleEvent(msg);
				}
			} catch (final OtpErlangException e) {
				done = this.fHandler.exception(e);
			}

		}
	}

	public void start() {
		this.t = new Thread(this);
		this.t.setPriority(3); // low
		this.t.setDaemon(true);
		this.t.start();
	}

	public void stop() {
		this.t.interrupt();
	}

}
