package org.erlide.jinterface.rpc;

import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

public final class EventProcessor {

	private static final int TIMEOUT = 10000;

	private final EventHandler handler;
	private final OtpMbox mbox;
	private volatile boolean terminated = true;

	public EventProcessor(final EventHandler h, final OtpNode node) {
		handler = h;
		mbox = node.createMbox();
	}

	public void run() {
		terminated = false;
		while (!terminated) {
			OtpErlangObject msg;
			try {
				msg = mbox.receive(TIMEOUT);
				if (msg != null) {
					terminated = handler.handleEvent(msg);
				}
			} catch (final OtpErlangExit e) {
				terminated = true;
			} catch (final Exception e) {
				ErlLogger.warn("EventProcessor %s got exception:", mbox.self());
				ErlLogger.warn(e);
			}

		}
	}

	public void start() {
		if (!terminated) {
			return;
		}
		Thread t = new Thread(new Runnable() {
			public void run() {
				EventProcessor.this.run();
			}
		});
		t.setPriority(3); // low
		t.setDaemon(true);
		t.start();
	}

	public void stop() {
		terminated = true;
	}

	public OtpErlangPid getPid() {
		return mbox.self();
	}

}
