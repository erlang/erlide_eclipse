package com.ericsson.otp.erlang;

public class OtpErlangPidProxy extends OtpErlangPid {

	private static final long serialVersionUID = 1L;

	private String node;

	public OtpErlangPidProxy(String node, OtpInputStream buf)
			throws OtpErlangDecodeException {
		super(buf);
		this.node = node;
	}

	@Override
	public String toString() {
		if (node.equals(node())) {
			return "#Pid<0." + id() + "." + serial() + ">";
		} else {
			return super.toString();
		}
	}

}
