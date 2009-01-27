package org.erlide.jinterface;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class JInterfaceFactory {

	public static OtpErlangList mkList(OtpErlangObject... args) {
		return new OtpErlangList(args);
	}

	public static OtpErlangTuple mkTuple(OtpErlangObject... args) {
		return new OtpErlangTuple(args);
	}

}
