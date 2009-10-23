package com.ericsson.otp.erlang;

import java.util.Collection;

public class JInterfaceFactory {

	public static OtpErlangList mkList(final OtpErlangObject... args) {
		return new OtpErlangList(args);
	}

	public static OtpErlangList mkList(final Collection<OtpErlangObject> args) {
		return new OtpErlangList(args.toArray(new OtpErlangObject[args.size()]));
	}

	public static OtpErlangTuple mkTuple(final OtpErlangObject... args) {
		return new OtpErlangTuple(args);
	}

}
