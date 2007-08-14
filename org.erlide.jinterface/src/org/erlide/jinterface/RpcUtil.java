/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.jinterface;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class RpcUtil {

	private static final boolean VERBOSE = true;
	private static Map<OtpErlangRef, Object> objects = new HashMap<OtpErlangRef, Object>();
	private static int refid = 1;

	@SuppressWarnings("boxing")
	public static OtpErlangObject java2erlang(Object o) {
		if (o instanceof OtpErlangObject) {
			return (OtpErlangObject) o;
		} else if (o instanceof String) {
			return new OtpErlangString((String) o);
		} else if (o instanceof Integer) {
			return new OtpErlangInt((Integer) o);
		} else if (o instanceof Long) {
			return new OtpErlangLong((Long) o);
		} else if (o instanceof Double) {
			return new OtpErlangDouble((Double) o);
		} else if (o instanceof Boolean) {
			return new OtpErlangAtom((Boolean) o ? "true" : "false");
		} else if (o instanceof List<?>) {
			Object[] v = ((List<?>) o).toArray(new Object[] {});
			OtpErlangObject[] vv = new OtpErlangObject[v.length];
			for (int i = 0; i < v.length; i++) {
				vv[i] = java2erlang(v[i]);
			}
			return new OtpErlangList(vv);
		} else if (o instanceof Object[]) {
			Object[] v = ((Object[]) o);
			OtpErlangObject[] vv = new OtpErlangObject[v.length];
			for (int i = 0; i < v.length; i++) {
				vv[i] = java2erlang(v[i]);
			}
			return new OtpErlangTuple(vv);
		} else {
			return registerTarget(o);
		}
	}

	@SuppressWarnings("boxing")
	public static Object erlang2java(OtpErlangObject o) {
		try {
			if (o instanceof OtpErlangString) {
				return ((OtpErlangString) o).stringValue();
			} else if (o instanceof OtpErlangInt) {
				return ((OtpErlangInt) o).intValue();
			} else if (o instanceof OtpErlangLong) {
				return ((OtpErlangLong) o).longValue();
			} else if (o instanceof OtpErlangDouble) {
				return ((OtpErlangDouble) o).doubleValue();
			} else if (o instanceof OtpErlangAtom) {
				String a = ((OtpErlangAtom) o).atomValue();
				if ("true".equals(a)) {
					return true;
				} else if ("false".equals(a)) {
					return false;
				} else {
					return 0;
				}
			} else if (o instanceof OtpErlangList) {
				OtpErlangObject[] v = ((OtpErlangList) o).elements();
				Object[] vv = new Object[v.length];
				for (int i = 0; i < v.length; i++) {
					vv[i] = erlang2java(v[i]);
				}
				return Arrays.asList(vv);
			} else if (o instanceof OtpErlangTuple) {
				OtpErlangObject[] v = ((OtpErlangTuple) o).elements();
				Object[] vv = new Object[v.length];
				for (int i = 0; i < v.length; i++) {
					vv[i] = erlang2java(v[i]);
				}
				return vv;
			} else {
				return o;
			}
		} catch (Exception e) {
			return o;
		}
	}

	public static OtpErlangRef registerTarget(Object o) {
		OtpErlangRef ref = mkref();
		objects.put(ref, o);
		System.out.println("    >>" + ref + " " + o);
		return ref;
	}

	public static Object getTarget(OtpErlangRef ref) {
		System.out.println("    <<" + ref + " " + objects.get(ref));
		return objects.get(ref);
	}

	public static void unregisterTarget(OtpErlangRef ref) {
		objects.remove(ref);
	}

	// TODO define an exception type here
	public static Class erlang2javaType(Object o) throws Exception {
		if (!(o instanceof OtpErlangObject)) {
			throw new Exception("unsupported erlang2java type");
		}
		if (o instanceof OtpErlangString) {
			return String.class;
		} else if (o instanceof OtpErlangInt) {
			return Integer.class;
		} else if (o instanceof OtpErlangLong) {
			return Long.class;
		} else if (o instanceof OtpErlangDouble) {
			return Double.class;
		} else if (o instanceof OtpErlangAtom) {
			return Boolean.class;
		} else if (o instanceof OtpErlangList) {
			return List.class;
		} else if (o instanceof OtpErlangTuple) {
			return Object[].class;
		} else if (o instanceof OtpErlangRef) {
			return objects.get(o).getClass();
		} else {
			return o.getClass();
		}
	}

	private static OtpErlangRef mkref() {
		return new OtpErlangRef("dummy_erlide", new int[] { refid++, refid++,
				refid++ }, 0);
	}

	public static void handleRequests(List<OtpErlangObject> msgs,
			final IRpcHandler rpcHandler) {
		if (msgs.size() == 0) {
			return;
		}
		for (OtpErlangObject msg : msgs) {
			try {
				OtpErlangTuple t = (OtpErlangTuple) msg;
				debug("-- RPC: " + msg);
				OtpErlangAtom kind = (OtpErlangAtom) t.elementAt(0);
				OtpErlangObject receiver = t.elementAt(1);
				OtpErlangAtom method = (OtpErlangAtom) t.elementAt(2);
				if ("call".equals(kind.atomValue())) {
					OtpErlangList args = buildArgs(t.elementAt(3));
					OtpErlangPid from = (OtpErlangPid) t.elementAt(4);

					OtpErlangObject result = execute(receiver, method, args
							.elements());
					rpcHandler.reply(from, result);

				} else if ("uicall".equals(kind.atomValue())) {
					OtpErlangPid from = (OtpErlangPid) t.elementAt(1);
					OtpErlangList args = buildArgs(t.elementAt(4));

					// TODO how to use Display.asyncExec() in a non-ui plugin?
					OtpErlangObject result = execute(receiver, method, args
							.elements());
					rpcHandler.reply(from, result);

				} else if ("cast".equals(kind.atomValue())) {
					OtpErlangList args = buildArgs(t.elementAt(3));

					execute(receiver, method, args.elements());

				} else if ("event".equals(kind.atomValue())) {
					String id = ((OtpErlangAtom) receiver).atomValue();

					rpcHandler.event(id, method);

				} else {
					log("unknown message type: " + msg);
				}
			} catch (Exception e) {
				log("strange message: " + msg);
				e.printStackTrace();
			}
		}
	}

	private static OtpErlangList buildArgs(OtpErlangObject a) throws Exception {
		final OtpErlangList args;
		if (a instanceof OtpErlangList) {
			args = (OtpErlangList) a;
		} else if (a instanceof OtpErlangString) {
			String ss = ((OtpErlangString) a).stringValue();
			byte[] bytes = ss.getBytes();
			OtpErlangObject[] str = new OtpErlangObject[ss.length()];
			for (int i = 0; i < ss.length(); i++) {
				str[i] = new OtpErlangInt(bytes[i]);
			}
			args = new OtpErlangList(str);
		} else {
			throw new Exception("bad RPC argument list: " + a);
		}
		return args;
	}

	private static OtpErlangObject execute(OtpErlangObject target,
			OtpErlangAtom method, OtpErlangObject[] args) {

		debug("EXEC:: " + target + ":" + method + " " + args + " >"
				+ (args == null ? 0 : args.length));

		Object[] parms;
		if (args != null) {
			parms = new Object[args.length];
			for (int i = 0; i < args.length; i++) {
				// parms[i] = args[i];
				parms[i] = RpcUtil.erlang2java(args[i]);
			}
		} else {
			parms = null;
		}

		if (target instanceof OtpErlangRef) {
			// object call
			Object rcvr = RpcUtil.getTarget((OtpErlangRef) target);
			if (rcvr != null) {
				try {
					return callMethod(rcvr, method.atomValue(), parms);
				} catch (Exception e) {
					log("bad RPC: " + e.getMessage());
					return new OtpErlangTuple(new OtpErlangObject[] {
							new OtpErlangAtom("error"),
							new OtpErlangString(String.format("Bad RPC: %s", e
									.getMessage())) });
				}

			} else {
				log("RPC: unknown receiver: " + target);
				return new OtpErlangTuple(new OtpErlangObject[] {
						new OtpErlangAtom("error"),
						new OtpErlangString(String.format(
								"Bad RPC: unknown object ref %s%n", target)) });
			}

		} else if (target instanceof OtpErlangAtom) {
			// static call
			String clazzName = ((OtpErlangAtom) target).atomValue();
			try {
				Class clazz = Class.forName(clazzName);
				return callMethod(clazz, method.atomValue(), parms);
			} catch (Exception e) {
				log("bad RPC: " + e.getMessage());
				return new OtpErlangTuple(new OtpErlangObject[] {
						new OtpErlangAtom("error"),
						new OtpErlangString(String.format("Bad RPC: %s", e
								.getMessage())) });
			}
		} else {
			log("unknown receiver: " + target);
			return new OtpErlangTuple(new OtpErlangObject[] {
					new OtpErlangAtom("error"),
					new OtpErlangString(String.format(
							"Bad RPC: unknown receiver %s", target)) });
		}
	}

	private static OtpErlangObject callMethod(Object rcvr, String method,
			Object[] args) throws Exception {
		Class cls = (rcvr instanceof Class) ? (Class) rcvr : rcvr.getClass();
		Method meth; // = invoke(cls, args);
		boolean many;
		try {
			Class[] params;
			if (args != null) {
				params = new Class[args.length];
				for (int i = 0; i < args.length; i++) {
					// params[i] = Class
					// .forName("com.ericsson.otp.erlang.OtpErlangObject");
					params[i] = args[i].getClass();
					// params[i] =ErlUtils.erlang2javaType(args[i]);
					System.out.println("? " + i + " "
							+ args[i].getClass().getName() + " : "
							+ params[i].getName());
				}
			} else {
				params = null;
			}
			meth = cls.getMethod(method, params);
			many = false;
		} catch (NoSuchMethodException e) {
			try {
				Class[] params;
				if (args != null) {
					params = new Class[args.length];
					for (int i = 0; i < args.length; i++) {
						params[i] = Class.forName("java.lang.Object");
						// params[i] = args[i].getClass();
						// params[i] =ErlUtils.erlang2javaType(args[i]);
						System.out.println("? " + i + " "
								+ args[i].getClass().getName() + " : "
								+ params[i].getName());
					}
				} else {
					params = null;
				}
				meth = cls.getMethod(method, params);
				many = false;
			} catch (NoSuchMethodException ee) {
				meth = cls.getMethod(method, Class
						.forName("[Ljava.lang.Object;"));
				many = true;
			}
		}
		try {
			// meth.setAccessible(true);
			System.out.println("&& " + many + " " + meth.isVarArgs());
			Object o = many ? meth.invoke(rcvr, (Object) args) : meth.invoke(
					rcvr, args);
			if (VERBOSE) {
				log(String.format("** %s() returned %s", meth, o));
			}

			return java2erlang(o);

			// Handle any exceptions thrown by method to be invoked.
		} catch (InvocationTargetException x) {
			Throwable cause = x.getCause();
			log(String.format("invocation of %s failed: %s", meth, cause
					.getMessage()));
			return new OtpErlangTuple(new OtpErlangObject[] {
					new OtpErlangAtom("error"),
					new OtpErlangString(String.format(
							"invocation of %s failed: %s", meth, cause
									.getMessage())) });
		}
	}

	private static void log(String s) {
		System.out.println("ErlUtils: " + s);
	}

	private static void debug(String s) {
		if (VERBOSE)
			System.out.println("ErlUtils: " + s);
	}

	// /////////////////////// test rpc from erlang
	public static Object testing() {
		HashMap<String, String> m = new HashMap<String, String>();
		m.put("alfa", "beta");
		return m;
	}

	public static Object[] testing(Object arg1) {
		return new Object[] { 64, "hej", arg1 };
	}

	public static String testing(String arg1) {
		return arg1 + arg1;
	}

	public static Object testing(Object arg1, Object arg2) {
		return arg1;
	}

	public static Object testing(Object... args) {
		return args;
	}

}
