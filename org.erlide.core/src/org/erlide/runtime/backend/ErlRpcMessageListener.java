package org.erlide.runtime.backend;

import java.util.List;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface ErlRpcMessageListener {
	/**
	 * Handle messages and remove them if handled
	 * 
	 * @param msgs
	 *            List of msgs to handle/remove
	 */
	void handleMsgs(List<OtpErlangObject> msgs);
}
