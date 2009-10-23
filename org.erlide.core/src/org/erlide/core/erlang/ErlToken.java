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

import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public final class ErlToken {

	private static final boolean TRACE = false;

	private final String kind;
	private int length;
	private int offset;
	private int line;
	private String text;
	// private final OtpErlangTuple fTuple;

	public static final ErlToken EOF = new ErlToken();

	public ErlToken(final OtpErlangTuple e) {
		// fTuple = e;
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
			ErlLogger.warn(e1);
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
			// ErlLogger.debug("mkTok " + kind + " - " + line + "/" + offset +
			// ":"
			// + length + " " + (offset + length));
			ErlLogger.debug("mkTok " + kind + " - " + text + " " + line + "/"
					+ offset + ": '" + text + "' ");
		}
	}

	// eof token
	private ErlToken() {
		kind = null;
		// fTuple = null;
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
		return "{" + kind + ", " + line + "/" + offset + "+" + length + ": "
				+ text + "}";
	}

	/**
	 * @param ofs
	 */
	public void fixOffset(final int ofs) {
		offset += ofs;
	}

}
