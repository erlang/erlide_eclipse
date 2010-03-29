/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.jinterface.rpc.generator;

import java.io.IOException;
import java.util.List;

import org.erlide.jinterface.util.ErlLogger;
import org.erlide.jinterface.util.TypeConverter;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.erlang.SignatureException;

/**
 * Takes a module name and generates a java interface that can be used with
 * ErlangRpcProxy to call the Erlang code.
 * 
 * The interface uses by default OtpErlangObjects, but one can choose to make it
 * rely on RpcUtil.java2Erlang
 * 
 * The result is a java source file.
 * 
 */
public class RpcInterfaceGenerator {

	@SuppressWarnings("unchecked")
	public static void main(final String[] args) {
		try {
			final OtpNode node = new OtpNode("dummy");

			final OtpMbox mbox = node.createMbox();
			OtpErlangObject res = null;
			try {
				// try {
				// OtpErlangObject msg = RpcUtil
				// .rpcCall(mbox.self(), "erlang", "module_info",
				// new OtpErlangObject[] { new OtpErlangAtom(
				// "exports") });
				// mbox.send("rex", "wolf", msg);
				// } finally {
				// node.closeMbox(mbox);
				// }
				res = mbox.receive(1000);
			} catch (final OtpErlangExit e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (final OtpErlangDecodeException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (res == null) {
				return;
			}
			final OtpErlangTuple r = (OtpErlangTuple) res;
			res = r.elementAt(1);
			List<OtpErlangObject> l = null;
			try {
				l = (List<OtpErlangObject>) TypeConverter.erlang2java(res,
						List.class);
				ErlLogger.debug(generate(node, "erlang",
						"org.erlide.jinterface.erlangrpc", false, l));

			} catch (final SignatureException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} catch (final IOException e) {
			e.printStackTrace();
		}
	}

	public static String generate(final OtpNode node, final String module,
			final String pkg, final boolean convert,
			final List<OtpErlangObject> l) {
		final StringBuilder b = new StringBuilder();
		try {
			generate(b, module, pkg, convert, l);
		} catch (final OtpErlangRangeException e) {
			e.printStackTrace();
		}
		return b.toString();
	}

	private static void generate(final StringBuilder b, final String module,
			final String pkg, final boolean convert,
			final List<OtpErlangObject> l) throws OtpErlangRangeException {
		final String cls = toJavaClassName(module);

		b.append("package " + pkg + ";\n");
		b.append("\n");

		b.append("import com.ericsson.otp.erlang.*;\n");
		b.append("\n");

		b.append("public interface " + cls + " {\n");

		for (final OtpErlangObject item : l) {
			final OtpErlangTuple t = (OtpErlangTuple) item;
			final String name = ((OtpErlangAtom) t.elementAt(0)).atomValue();
			final int arity = ((OtpErlangLong) t.elementAt(1)).intValue();
			b.append("  OtpErlangObject " + name + "(");
			for (int i = 0; i < arity; i++) {
				b.append("OtpErlangObject arg" + i);
				if (i < arity - 1) {
					b.append(", ");
				}
			}
			b.append(");\n");
		}

		b.append("}\n");

		b.append("////////////////////////////////////////////////////\n");
	}

	private static String toJavaClassName(final String module) {
		return module;
		// module.substring(0, 1).toUpperCase() + module.substring(1);
	}

	public static Class<?> javaType2erlang(final Class<?> obj) {
		if (obj.isArray()) {
			return OtpErlangTuple.class;
		}
		if (List.class.isAssignableFrom(obj)) {
			return OtpErlangList.class;
		}
		if (obj == Integer.TYPE) {
			return OtpErlangLong.class;
		}
		if (obj == Long.TYPE) {
			return OtpErlangLong.class;
		}
		if (obj == Boolean.TYPE) {
			return OtpErlangAtom.class;
		}
		if (obj == Double.TYPE) {
			return OtpErlangDouble.class;
		}
		if (obj == String.class) {
			return OtpErlangString.class;
		}
		if (obj == Long.class) {
			return OtpErlangLong.class;
		}
		if (obj == Integer.class) {
			return OtpErlangLong.class;
		}
		if (obj == Double.class) {
			return OtpErlangDouble.class;
		}
		if (obj == Boolean.class) {
			return OtpErlangAtom.class;
		}
		return OtpErlangRef.class;
	}
}
