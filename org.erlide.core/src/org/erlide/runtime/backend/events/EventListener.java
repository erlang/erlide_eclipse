package org.erlide.runtime.backend.events;

import java.util.List;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface EventListener {
	/**
	 * Handle messages and remove them if handled
	 * 
	 * @param msgs
	 *            List of msgs to handle/remove
	 */
	void handleMsgs(List<OtpErlangObject> msgs);

	boolean handleMsg(OtpErlangObject msg);

	boolean match_id(OtpErlangObject id);
}
