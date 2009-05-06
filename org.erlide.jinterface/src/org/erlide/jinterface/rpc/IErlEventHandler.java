package org.erlide.jinterface.rpc;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;

public interface IErlEventHandler {

	void init();

	/**
	 * Handle events from erlang
	 * 
	 * @param msg
	 *            The term sent from erlang, can't be null
	 * 
	 */
	void handleEvent(OtpErlangObject msg);

	boolean isTerminated();

	OtpErlangObject receiveEvent(int timeout) throws OtpErlangExit,
	OtpErlangDecodeException;

	int getTimeout();

	boolean exception(Exception e);
}
