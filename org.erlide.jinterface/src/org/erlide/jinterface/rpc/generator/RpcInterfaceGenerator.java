/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.jinterface.rpc.generator;

import java.io.IOException;
import java.util.List;

import org.erlide.jinterface.rpc.RpcConverter;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.RpcUtil;

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
	public static void main(String[] args) {
		try {
			OtpNode node = new OtpNode("dummy");

			OtpMbox mbox = node.createMbox();
			OtpErlangObject msg = RpcUtil.buildRpcCall("erlang", "module_info",
					new OtpErlangObject[] { new OtpErlangAtom("exports") },
					mbox.self());
			mbox.send("rex", "wolf", msg);
			OtpErlangObject res = null;
			try {
				res = mbox.receive(1000);
			} catch (OtpErlangExit e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (OtpErlangDecodeException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			OtpErlangTuple r = (OtpErlangTuple) res;
			res = r.elementAt(1);
			List<OtpErlangObject> l = null;
			try {
				l = (List<OtpErlangObject>) RpcConverter.erlang2java(res,
						List.class);
				System.out.println(generate(node, "erlang",
						"org.erlide.jinterface.erlangrpc", false, l));

			} catch (RpcException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public static String generate(OtpNode node, String module, String pkg,
			boolean convert, List<OtpErlangObject> l) {
		StringBuilder b = new StringBuilder();
		try {
			generate(b, module, pkg, convert, l);
		} catch (OtpErlangRangeException e) {
			e.printStackTrace();
		}
		return b.toString();
	}

	private static void generate(StringBuilder b, String module, String pkg,
			boolean convert, List<OtpErlangObject> l)
			throws OtpErlangRangeException {
		String cls = toJavaClassName(module);

		b.append("package " + pkg + ";\n");
		b.append("\n");

		b.append("import com.ericsson.otp.erlang.*;\n");
		b.append("\n");

		b.append("public interface " + cls + " {\n");

		for (OtpErlangObject item : l) {
			OtpErlangTuple t = (OtpErlangTuple) item;
			String name = ((OtpErlangAtom) t.elementAt(0)).atomValue();
			int arity = ((OtpErlangLong) t.elementAt(1)).intValue();
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

	private static String toJavaClassName(String module) {
		return module;
		// module.substring(0, 1).toUpperCase() + module.substring(1);
	}

	public static Class<?> javaType2erlang(Class<?> obj) {
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
