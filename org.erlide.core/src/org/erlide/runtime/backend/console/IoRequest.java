/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend.console;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class IoRequest {
	private OtpErlangPid leader;
	private OtpErlangPid sender;
	private String message;
	private int start;
	private final boolean output;

	public IoRequest(final OtpErlangTuple obj) {
		try {
			final OtpErlangObject o = obj.elementAt(0);
			if (o instanceof OtpErlangString) {
				message = ((OtpErlangString) o).stringValue();
			} else if (o instanceof OtpErlangList) {
				final OtpErlangList l = (OtpErlangList) o;
				if (l.arity() == 0) {
					message = "";
				} else {
					message = l.toString();
				}
			} else {
				message = o.toString();
			}

			leader = (OtpErlangPid) obj.elementAt(1);
			final OtpErlangObject s = obj.elementAt(2);
			if (s instanceof OtpErlangPid) {
				sender = (OtpErlangPid) s;
			} else {
				sender = new OtpErlangPid("s", 0, 0, 0);
			}
		} catch (final Exception e) {
			message = null;
		}
		output = true;
	}

	// used for input text
	public IoRequest(final String msg) {
		message = msg;
		leader = new OtpErlangPid("s", 0, 0, 0);
		sender = new OtpErlangPid("s", 0, 0, 0);
		output = false;
	}

	@Override
	public String toString() {
		if (output) {
			return "{out:: '" + message + "', " + start + "/"
					+ message.length() + ", " + leader + ", " + sender + "}";
		} else {
			return "{in:: '" + message + "'}";
		}
	}

	public OtpErlangPid getLeader() {
		return leader;
	}

	public String getMessage() {
		return message;
	}

	public OtpErlangPid getSender() {
		return sender;
	}

	public int getStart() {
		return start;
	}

	public int getLength() {
		return message.length();
	}

	public boolean isOutput() {
		return output;
	}

	public void setStart(int pos) {
		start = pos;
	}
}
