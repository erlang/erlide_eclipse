package com.ericsson.otp.erlang;


public class JInterfaceFactory {

	public static OtpErlangList mkList(final OtpErlangObject... args) {
		return new OtpErlangList(args);
	}

	public static OtpErlangTuple mkTuple(final OtpErlangObject... args) {
		return new OtpErlangTuple(args);
	}

}
