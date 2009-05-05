/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.jinterface.rpc;

import org.erlide.jinterface.Bindings;
import org.erlide.jinterface.ErlUtils;
import org.erlide.jinterface.JInterfaceFactory;
import org.erlide.jinterface.ParserException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;

/**
 * This is a thread implementing an Erlang io server.
 */
public class IOServer implements Runnable {

	private final IOCallback callback;
	private final OtpMbox mbox;

	public static enum Encoding {
		latin1, unicode;
	}

	public IOServer(OtpMbox box, IOCallback callback) {
		this.callback = callback;
		mbox = box;
		new Thread(this, "io_server");
	}

	public void run() {
		boolean done = false;
		do {
			OtpErlangObject msg;
			try {
				msg = mbox.receive(3000);
				if (msg != null) {

					System.out.println("IOS "
							+ Thread.currentThread().getName() + " : " + msg);

					if (msg instanceof OtpErlangTuple) {
						OtpErlangTuple tuple = (OtpErlangTuple) msg;
						String tag = ((OtpErlangAtom) tuple.elementAt(0))
								.atomValue();
						if ("io_request".equals(tag)) {
							OtpErlangPid from = (OtpErlangPid) tuple
									.elementAt(1);
							OtpErlangObject replyAs = tuple.elementAt(2);
							OtpErlangTuple request = (OtpErlangTuple) tuple
									.elementAt(3);
							OtpErlangObject reply = processRequest(from,
									request);
							OtpErlangTuple replyMsg = JInterfaceFactory
									.mkTuple(new OtpErlangAtom("io_reply"),
											replyAs, reply);
							mbox.send(from, replyMsg);
						} else {
							System.out.println("IOServer: unknown message "
									+ msg);
						}
					} else {
						System.out.println("IOServer: unknown message " + msg);
					}
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		} while (!done || !Thread.interrupted());
	}

	private final OtpErlangObject error = JInterfaceFactory.mkTuple(
			new OtpErlangAtom("error"), new OtpErlangAtom("request"));

	private OtpErlangObject processRequest(OtpErlangPid from,
			OtpErlangObject arequest) {
		if (callback == null) {
			return error;
		}
		Bindings b;
		try {
			if (arequest instanceof OtpErlangTuple) {
				OtpErlangTuple request = (OtpErlangTuple) arequest;
				String tag = ((OtpErlangAtom) request.elementAt(0)).atomValue();
				if ("put_chars".equals(tag)) {
					b = ErlUtils.match("{put_chars, Chars}", request);
					if (b != null) {
						return callback.putChars(from, Encoding.latin1, b
								.get("Chars"));
					}

					b = ErlUtils.match("{put_chars, Enc:a, Chars}", request);
					if (b != null) {
						String enc = b.getAtom("Enc");
						return callback.putChars(from, Encoding.valueOf(enc), b
								.get("Chars"));
					}

					b = ErlUtils.match("{put_chars, M:a, F:a, A}", request);
					if (b != null) {
						String m = b.getAtom("M");
						String f = b.getAtom("F");
						OtpErlangObject[] a = b.getList("A");
						return callback
								.putChars(from, Encoding.latin1, m, f, a);
					}

					b = ErlUtils.match("{put_chars, Enc:a, M:a, F:a, A}",
							request);
					if (b != null) {
						String enc = b.getAtom("Enc");
						String m = b.getAtom("M");
						String f = b.getAtom("F");
						OtpErlangObject[] a = b.getList("A");
						return callback.putChars(from, Encoding.valueOf(enc),
								m, f, a);
					}
					return error;
				} else if ("get_until".equals(tag)) {
					b = ErlUtils.match("{get_until, Prompt}", request);
					if (b != null) {
						return callback.getUntil(Encoding.latin1, b
								.get("Prompt"));
					}
					b = ErlUtils.match("{get_until, Prompt, N:i}", request);
					if (b != null) {
						long n = b.getLong("N");
						return callback.getUntil(Encoding.latin1, b
								.get("Prompt"), n);
					}
					b = ErlUtils.match("{get_until, Enc:a, Prompt}", request);
					if (b != null) {
						String enc = b.getAtom("Enc");
						return callback.getUntil(Encoding.valueOf(enc), b
								.get("Prompt"));
					}
					b = ErlUtils.match("{get_until, Enc:a, Prompt, N:i}",
							request);
					if (b != null) {
						String enc = b.getAtom("Enc");
						long n = b.getLong("N");
						return callback.getUntil(Encoding.valueOf(enc), b
								.get("Prompt"), n);
					}
					b = ErlUtils.match("{get_until, Prompt, M:a, F:a, A}",
							request);
					if (b != null) {
						String m = b.getAtom("M");
						String f = b.getAtom("F");
						OtpErlangObject[] a = b.getList("A");
						return callback.getUntil(Encoding.latin1, b
								.get("Prompt"), m, f, a);
					}
					b = ErlUtils
							.match("{get_until, Enc: a, Prompt, M:a, F:a, A}",
									request);
					if (b != null) {
						String enc = b.getAtom("Enc");
						String m = b.getAtom("M");
						String f = b.getAtom("F");
						OtpErlangObject[] a = b.getList("A");
						return callback.getUntil(Encoding.valueOf(enc), b
								.get("Prompt"), m, f, a);
					}
				} else if ("requests".equals(tag)) {
					b = ErlUtils.match("{requests, Reqs:lx}", request);
					if (b != null) {
						OtpErlangObject[] reqs = b.getList("Reqs");
						OtpErlangObject val = null;
						for (OtpErlangObject r : reqs) {
							val = processRequest(from, r);
							if (val == error) {
								return error;
							}
						}
						return val == null ? error : val;
					}
				} else if ("setopts".equals(tag)) {
					b = ErlUtils.match("{setopts, Opts:lx}", request);
					if (b != null) {
						OtpErlangObject[] opts = b.getList("Opts");
						return callback.setOpts(opts);
					}
				} else if ("get_geometry".equals(tag)) {
					return JInterfaceFactory.mkTuple(
							new OtpErlangAtom("error"), new OtpErlangAtom(
									"enotsup"));
				} else {
					return error;
				}
			} else if (arequest instanceof OtpErlangAtom) {
				OtpErlangAtom tag = (OtpErlangAtom) arequest;
				if ("getopts".equals(tag)) {
					return callback.getOpts();
				} else {
					return error;
				}
			} else {
				return error;
			}
		} catch (ParserException e) {
			e.printStackTrace();
		} catch (OtpErlangException e) {
			e.printStackTrace();
		}
		return error;
	}
}
