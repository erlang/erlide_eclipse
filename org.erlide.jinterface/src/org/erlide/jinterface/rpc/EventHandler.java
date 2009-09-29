package org.erlide.jinterface.rpc;

import com.ericsson.otp.erlang.OtpErlangObject;

public abstract class EventHandler {

	/**
	 * Handle events from erlang. Return true to terminate. Should be stateless.
	 * 
	 * @param msg
	 *            The term sent from erlang, can't be null
	 * 
	 */
	public boolean handleEvent(OtpErlangObject msg) {
		// do nothing by default
		return false;
	}

}
