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

public class IoRequest implements Comparable<IoRequest> {

	public static class Timestamp implements Comparable<Timestamp> {
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
			long ts = System.currentTimeMillis();
			megasecs = ts / 1000000000;
			ts = ts - megasecs * 1000000000;
			secs = ts / 1000;
			ts = ts - secs * 1000;
			microsecs = ts * 1000;
		}

		public Timestamp(Timestamp ts) {
			megasecs = ts.megasecs;
			secs = ts.secs;
			microsecs = ts.microsecs;
		}

		@Override
		public String toString() {
			return "{" + megasecs + "," + secs + "," + microsecs + "}";
		}

		public int compareTo(Timestamp ts) {
			if (ts == null) {
				return -1;
			}
			if (ts.megasecs > megasecs) {
				return -1;
			} else if (ts.megasecs < megasecs) {
				return 1;
			} else {
				if (ts.secs > secs) {
					return -1;
				} else if (ts.secs < secs) {
					return 1;
				} else {
					if (ts.microsecs > microsecs) {
						return -1;
					} else if (ts.microsecs < microsecs) {
						return 1;
					} else {
					}
				}
			}
			return 0;
		}

		@Override
		public boolean equals(Object o) {
			if (!(o instanceof Timestamp)) {
				return false;
			}
			return compareTo((Timestamp) o) == 0;
		}

		public Timestamp next() {
			Timestamp r = new Timestamp(this);
			r.microsecs++;
			return r;
		}

	}

	private OtpErlangPid leader;

	private OtpErlangPid sender;

	private String message;

	private Timestamp tstamp;

	private int start;

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
			OtpErlangObject s = obj.elementAt(2);
			if (s instanceof OtpErlangPid) {
				sender = (OtpErlangPid) s;
			} else {
				sender = new OtpErlangPid("s", 0, 0, 0);
			}
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
		return leader;
	}

	public String getMessage() {
		return message;
	}

	public OtpErlangPid getSender() {
		return sender;
	}

	public Timestamp getTstamp() {
		return tstamp;
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

	public int compareTo(IoRequest io) {
		if (io == null) {
			return -1;
		}
		return tstamp.compareTo(io.getTstamp());
	}

}
