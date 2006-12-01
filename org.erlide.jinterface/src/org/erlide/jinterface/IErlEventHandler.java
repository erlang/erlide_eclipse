package org.erlide.jinterface;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;

public interface IErlEventHandler {

	void init();

	/**
	 * msg can't be null
	 */
	void handleEvent(OtpErlangObject msg);

	boolean isTerminated();

	OtpErlangObject receiveEvent(int timeout) throws OtpErlangExit,
			OtpErlangDecodeException;

	int getTimeout();

	boolean exception(Exception e);
}
