package com.ericsson.otp.erlang;

public class OtpErlangPidProxy extends OtpErlangPid {

	private static final long serialVersionUID = 1L;

	public OtpErlangPidProxy(OtpInputStream buf)
			throws OtpErlangDecodeException {
		super(buf);
	}

	@Override
	public String toString() {
		return "#Pid<0." + id() + "." + serial() + ">";
	}

}
