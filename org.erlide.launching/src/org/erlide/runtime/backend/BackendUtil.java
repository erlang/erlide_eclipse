/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend;

import java.io.IOException;
import java.net.URL;

import org.eclipse.core.runtime.FileLocator;
import org.erlide.basiccore.ErlLogger;
import org.erlide.jinterface.RpcStubGenerator;
import org.erlide.jinterface.RpcUtil;
import org.erlide.runtime.ErlangLaunchPlugin;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangEvalException;
import org.erlide.runtime.backend.exceptions.ErlangParseException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * 
 * 
 * @author Vlad Dumitrescu [vlad_dumitrescu at hotmail dot com]
 */
public class BackendUtil {

	public static String format(IBackend b, String fmt, OtpErlangObject arg) {
		return format(b, fmt, arg);
	}

	public static String format(IBackend b, String fmt, OtpErlangObject... args) {
		final OtpErlangObject[] args1 = new OtpErlangObject[2];
		args1[0] = new OtpErlangString(fmt);
		args1[1] = new OtpErlangList(args);

		try {
			final String r = b.rpc(IBackend.ERL_BACKEND, "format",
					(Object) args1).toString();
			return r.substring(1, r.length() - 1);
		} catch (final ErlangRpcException e) {
			e.printStackTrace();
		}
		return "error";
	}

	/**
	 * @param string
	 * @return OtpErlangobject
	 * @throws ErlangParseException
	 */
	public static OtpErlangObject parseTerm(IBackend b, String string)
			throws ErlangParseException {
		OtpErlangObject r1 = null;
		try {
			r1 = b.rpcx(IBackend.ERL_BACKEND, "parse_term",
					new OtpErlangString(string));
		} catch (final Exception e) {
			throw new ErlangParseException("Could not parse term \"" + string
					+ "\"");
		}
		final OtpErlangTuple t1 = (OtpErlangTuple) r1;

		if (((OtpErlangAtom) t1.elementAt(0)).atomValue().compareTo("ok") == 0) {
			return t1.elementAt(1);
		}
		throw new ErlangParseException("Could not parse term \"" + string
				+ "\": " + t1.elementAt(1).toString());
	}

	/**
	 * @param string
	 * @return
	 * @throws BackendException
	 */
	public static OtpErlangObject scanString(IBackend b, String string)
			throws BackendException {
		OtpErlangObject r1 = null;
		try {
			r1 = b.rpcx(IBackend.ERL_BACKEND, "scan_string",
					new OtpErlangString(string));
		} catch (final Exception e) {
			throw new BackendException("Could not tokenize string \"" + string
					+ "\": " + e.getMessage());
		}
		final OtpErlangTuple t1 = (OtpErlangTuple) r1;

		if (((OtpErlangAtom) t1.elementAt(0)).atomValue().compareTo("ok") == 0) {
			return t1.elementAt(1);
		}
		throw new BackendException("Could not tokenize string \"" + string
				+ "\": " + t1.elementAt(1).toString());
	}

	/**
	 * @param string
	 * @return
	 * @throws BackendException
	 */
	public static OtpErlangObject parseString(IBackend b, String string)
			throws BackendException {
		OtpErlangObject r1 = null;
		try {
			r1 = b.rpcx(IBackend.ERL_BACKEND, "parse_string",
					new OtpErlangString(string));
		} catch (final BackendException e) {
			throw new BackendException("Could not parse string \"" + string
					+ "\": " + e.getMessage());
		}
		final OtpErlangTuple t1 = (OtpErlangTuple) r1;

		if (((OtpErlangAtom) t1.elementAt(0)).atomValue().compareTo("ok") == 0) {
			return t1.elementAt(1);
		}
		throw new BackendException("Could not parse string \"" + string
				+ "\": " + t1.elementAt(1).toString());
	}

	public static OtpErlangObject ok(OtpErlangObject v0) {
		if (!(v0 instanceof OtpErlangTuple)) {
			return v0;
		}

		final OtpErlangTuple v = (OtpErlangTuple) v0;
		if (((OtpErlangAtom) v.elementAt(0)).atomValue().compareTo("ok") == 0) {
			return v.elementAt(1);
		}
		return v;
	}

	public static String prettyPrint(IBackend b, String text)
			throws BackendException {
		OtpErlangObject r1 = null;
		try {
			r1 = b.rpcx("erlide_backend", "pretty_print", new OtpErlangString(
					text + "."));
		} catch (final BackendException e) {
			throw new BackendException("Could not parse string \"" + text
					+ "\": " + e.getMessage());
		}
		return ((OtpErlangString) r1).stringValue();
	}

	/**
	 * 
	 * @param string
	 * @return OtpErlangObject
	 * @throws ErlangEvalException
	 */
	public static BackendEvalResult eval(IBackend b, String string) {
		return eval(b, string, null);
	}

	/**
	 * @param scratch
	 * @param bindings
	 * @return
	 */
	public static BackendEvalResult eval(IBackend b, String string,
			OtpErlangObject bindings) {
		final BackendEvalResult result = new BackendEvalResult();
		OtpErlangObject r1;
		try {
			OtpErlangObject[] args;
			if (bindings == null) {
				args = new OtpErlangObject[] { new OtpErlangString(string) };
			} else {
				args = new OtpErlangObject[] { new OtpErlangString(string),
						bindings };
			}

			r1 = b.rpcx(IBackend.ERL_BACKEND, "eval", (Object) args);
			// value may be something else if exception is thrown...
			final OtpErlangTuple t = (OtpErlangTuple) r1;
			final boolean ok = !"error".equals(((OtpErlangAtom) t.elementAt(0))
					.atomValue());
			if (ok) {
				result.setValue(t.elementAt(1), t.elementAt(2));
			} else {
				result.setError(t.elementAt(1));
			}
		} catch (final Exception e) {
			result.setError("rpc failed");
		}
		return result;
	}

	public static String getLocalPath(String string) {
		String dir = "?";
		final Bundle b = ErlangLaunchPlugin.getDefault().getBundle();
		final URL url = b.getEntry(string);
		try {
			dir = FileLocator.toFileURL(url).getPath().substring(1);
		} catch (final IOException e) {
			dir = "!?";
			e.printStackTrace();
		}
		return dir;
	}

	public static void generateRpcStub(String className, boolean onlyDeclared,
			IBackend b) {
		generateRpcStub(RpcUtil.getClassByName(className), onlyDeclared, b);
	}

	public static void generateRpcStub(Class cls, boolean onlyDeclared,
			IBackend b) {
		try {
			String s = RpcStubGenerator.generate(cls, onlyDeclared);
			RpcResult r = b.rpc("erlide_backend", "compile_string", s);
			if (!r.isOk()) {
				ErlLogger.debug("rpcstub::" + r.toString());
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}
