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

package org.erlide.core.erlang;

import java.text.DecimalFormat;

import org.erlide.basiccore.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangChar;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlToken {

	private static final boolean TRACE = false;

	private final String kind;

	private int length;

	private int offset;

	private int line;

	private String text;

	private final OtpErlangTuple fTuple;

	public static final ErlToken EOF = new ErlToken();

	public ErlToken(OtpErlangTuple e) {
		fTuple = e;
		kind = ((OtpErlangAtom) (e.elementAt(0))).atomValue();
		if (TRACE) {
			ErlLogger.debug("    =" + e.toString());
		}
		try {
			if (e.elementAt(1) instanceof OtpErlangTuple) {
				OtpErlangTuple pos;
				pos = (OtpErlangTuple) e.elementAt(1);
				// length = ((OtpErlangLong) pos.elementAt(1)).intValue() - 1;
				length = ((OtpErlangLong) pos.elementAt(1)).intValue();
				pos = (OtpErlangTuple) pos.elementAt(0);
				offset = ((OtpErlangLong) pos.elementAt(1)).intValue() - 1;
			} else {
				offset = ((OtpErlangLong) e.elementAt(1)).intValue() - 1;
			}
		} catch (final OtpErlangRangeException e1) {
			e1.printStackTrace();
			offset = 0;
		}
		OtpErlangObject ee = null;
		if (e.arity() > 3) {
			ee = e.elementAt(3);
		} else if (e.arity() > 2) {
			ee = e.elementAt(2);
		}
		if (ee != null) {
			if (TRACE) {
				ErlLogger.debug("   -" + ee.toString() + " "
						+ ee.getClass().getName());
			}
			if (ee instanceof OtpErlangString) {
				text = ((OtpErlangString) ee).stringValue();
			} else if (ee instanceof OtpErlangAtom) {
				text = ((OtpErlangAtom) ee).atomValue();
			} else if (ee instanceof OtpErlangLong) {
				text = ((OtpErlangLong) ee).toString();
			} else if (ee instanceof OtpErlangDouble) {
				text = new DecimalFormat().format(((OtpErlangDouble) ee)
						.doubleValue());
			} else if (ee instanceof OtpErlangChar) {
				try {
					text = new StringBuilder().append(
							((OtpErlangChar) ee).charValue()).toString();
				} catch (final OtpErlangRangeException e1) {
					text = "";
				}
			} else
			// might be a list of ints instead of a string
			{
				final OtpErlangObject[] ems = ((OtpErlangList) ee).elements();
				final StringBuilder buf = new StringBuilder(ems.length);
				for (OtpErlangObject element : ems) {
					try {
						buf.append(((OtpErlangLong) element).intValue());
					} catch (final OtpErlangRangeException e1) {
						e1.printStackTrace();
					}
				}
				text = buf.toString();
			}
		} else {
			text = kind;
		}
		if (TRACE) {
			ErlLogger.debug("mkTok " + kind + " - " + text + " " + offset + ":"
					+ text.length() + " " + (offset + text.length()));
		}
	}

	public ErlToken(OtpErlangTuple e, int x) {
		fTuple = e;
		if (TRACE) {
			ErlLogger.debug("    =" + e.toString());
		}

		final OtpErlangObject[] parts = e.elements();

		// -record(token, {kind, line, offset, length, value, text}).

		kind = ((OtpErlangAtom) parts[1]).atomValue();
		if ("eof".equals(kind)) {
			return;
		}

		try {
			line = ((OtpErlangLong) parts[2]).intValue();
			offset = ((OtpErlangLong) parts[3]).intValue();
			length = ((OtpErlangLong) parts[4]).intValue();
		} catch (final OtpErlangRangeException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		// value = ((OtpErlangAtom) parts[5]).atomValue();
		if (parts[6] instanceof OtpErlangString) {
			text = ((OtpErlangString) parts[6]).stringValue();
		} else {
			text = parts[6].toString();
			if ("undefined".equals(text)) {
				text = parts[5].toString();
			}
		}

		if (TRACE) {
			ErlLogger.debug("mkTok " + kind + " - " + text + " " + line + "/"
					+ offset + ":" + text.length() + " "
					+ (offset + text.length()));
		}
	}

	// eof token
	private ErlToken() {
		kind = null;
		fTuple = null;
	}

	/*
	 * public ErlToken(String k, String c) { kind = k; text = c; fTuple = null; }
	 */
	public String getContent() {
		return text;
	}

	public String getKind() {
		return kind;
	}

	public int getLength() {
		return length;
	}

	public int getOffset() {
		return offset;
	}

	@Override
	public String toString() {
		return "{" + kind + ", " + line + "/" + offset + ", '" + text + "'}";
	}

	/**
	 * @return Returns the fTuple.
	 */
	public OtpErlangTuple getTuple() {
		return fTuple;
	}

	public void fixOffset(int ofs) {
		offset += ofs;
	}

}
