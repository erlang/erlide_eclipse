package org.erlide.core.erlang;

import com.ericsson.otp.erlang.OtpErlangList;

public final class TokenWindow {

	private OtpErlangList fList;

	private int fPos;

	public OtpErlangList getList() {
		return fList;
	}

	public int getPos() {
		return fPos;
	}

	/**
	 * @param list
	 * @param pos
	 */
	public TokenWindow(OtpErlangList list, int pos) {
		super();
		fList = list;
		fPos = pos;
	}

}
