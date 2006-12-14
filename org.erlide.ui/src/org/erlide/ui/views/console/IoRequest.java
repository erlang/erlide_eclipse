/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.views.console;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class IoRequest {

	private OtpErlangPid leader;

	private OtpErlangPid sender;

	private String message;

	private Timestamp tstamp;

	private int start;

	public class Timestamp {
		public long megasecs;

		public long secs;

		public long microsecs;

		public Timestamp(OtpErlangTuple obj) {
			try {
				megasecs = ((OtpErlangLong) (obj.elementAt(0))).longValue();
				secs = ((OtpErlangLong) (obj.elementAt(1))).longValue();
				microsecs = ((OtpErlangLong) (obj.elementAt(2))).longValue();
			} catch (OtpErlangRangeException e) {
			}
		}

		Timestamp() {
		}

		@Override
		public String toString() {
			return "{" + megasecs + "," + secs + "," + microsecs + "}";
		}
	}

	public IoRequest(OtpErlangTuple obj) {

		try {
			OtpErlangObject o = obj.elementAt(0);
			if (o instanceof OtpErlangString) {
				message = ((OtpErlangString) o).stringValue();
			} else if (o instanceof OtpErlangList) {
				OtpErlangList l = (OtpErlangList) o;
				if (l.arity() == 0) {
					message = "";
				} else {
					message = l.toString();
				}
			} else {
				message = o.toString();
			}

			leader = (OtpErlangPid) obj.elementAt(1);
			sender = (OtpErlangPid) obj.elementAt(2);
			tstamp = new Timestamp((OtpErlangTuple) obj.elementAt(3));
		} catch (Exception e) {
			message = null;
		}
	}

	// used for input text
	public IoRequest(String msg) {
		message = msg;
		tstamp = new Timestamp();
		leader = new OtpErlangPid("s", 0, 0, 0);
		sender = new OtpErlangPid("s", 0, 0, 0);
	}

	@Override
	public String toString() {
		return "{'" + message + "', " + leader + ", " + sender + "}";
	}

	public OtpErlangPid getLeader() {
		return this.leader;
	}

	public String getMessage() {
		return this.message;
	}

	public OtpErlangPid getSender() {
		return this.sender;
	}

	public Timestamp getTstamp() {
		return this.tstamp;
	}

	void setLeader(OtpErlangPid leader) {
		this.leader = leader;
	}

	void setMessage(String message) {
		this.message = message;
	}

	void setSender(OtpErlangPid sender) {
		this.sender = sender;
	}

	void setTstamp(Timestamp tstamp) {
		this.tstamp = tstamp;
	}

	public void setStart(int start) {
		this.start = start;
	}

	public int getStart() {
		return start;
	}

	public int getLength() {
		return message.length();
	}

}
