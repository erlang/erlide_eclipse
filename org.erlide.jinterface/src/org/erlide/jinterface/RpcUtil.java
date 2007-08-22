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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
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

	private static class MethodDescription {
		public MethodDescription(String meth, Class<?>[] args) {
			name = meth;
			argTypes = args;
		}

		String name;
		Class<?>[] argTypes;
	}

	private static final boolean VERBOSE = false;

	private static Map<OtpErlangRef, Object> objects = new HashMap<OtpErlangRef, Object>();
	private static int refid = 1;

	// eclipse uses different classloaders for each plugin. this one is non-ui
	// so we have to set it from a ui one (when that one is initialized) so that
	// we can access even the UI classes (which are actually most interesting)
	public static ClassLoader loader = RpcUtil.class.getClassLoader();

	@SuppressWarnings("boxing")
	public static OtpErlangObject java2erlang(Object obj) {
		if (obj instanceof OtpErlangObject) {
			return (OtpErlangObject) obj;
		} else if (obj instanceof String) {
			return new OtpErlangString((String) obj);
		} else if (obj instanceof Integer) {
			return new OtpErlangInt((Integer) obj);
		} else if (obj instanceof Long) {
			return new OtpErlangLong((Long) obj);
		} else if (obj instanceof Double) {
			return new OtpErlangDouble((Double) obj);
		} else if (obj instanceof Boolean) {
			return new OtpErlangAtom((Boolean) obj ? "true" : "false");
		} else if (obj instanceof List<?>) {
			Object[] v = ((List<?>) obj).toArray(new Object[] {});
			OtpErlangObject[] vv = new OtpErlangObject[v.length];
			for (int i = 0; i < v.length; i++) {
				vv[i] = java2erlang(v[i]);
			}
			return new OtpErlangList(vv);
		} else if (obj instanceof Object[]) {
			Object[] v = ((Object[]) obj);
			OtpErlangObject[] vv = new OtpErlangObject[v.length];
			for (int i = 0; i < v.length; i++) {
				vv[i] = java2erlang(v[i]);
			}
			return new OtpErlangTuple(vv);
		} else {
			return registerTarget(obj);
		}
	}

	@SuppressWarnings("boxing")
	public static Object erlang2java(OtpErlangObject obj) {
		try {
			if (obj instanceof OtpErlangString) {
				return ((OtpErlangString) obj).stringValue();
			} else if (obj instanceof OtpErlangInt) {
				return ((OtpErlangInt) obj).intValue();
			} else if (obj instanceof OtpErlangLong) {
				return ((OtpErlangLong) obj).longValue();
			} else if (obj instanceof OtpErlangDouble) {
				return ((OtpErlangDouble) obj).doubleValue();
			} else if (obj instanceof OtpErlangAtom) {
				String a = ((OtpErlangAtom) obj).atomValue();
				if ("true".equals(a)) {
					return true;
				} else if ("false".equals(a)) {
					return false;
				} else {
					return obj;
				}
			} else if (obj instanceof OtpErlangList) {
				OtpErlangObject[] v = ((OtpErlangList) obj).elements();
				if (v != null) {
					Object[] vv = new Object[v.length];
					for (int i = 0; i < v.length; i++) {
						vv[i] = erlang2java(v[i]);
					}
					return Arrays.asList(vv);
				} else {
					return new ArrayList<Object>();
				}
			} else if (obj instanceof OtpErlangTuple) {
				OtpErlangObject[] v = ((OtpErlangTuple) obj).elements();
				if (v != null) {
					Object[] vv = new Object[v.length];
					for (int i = 0; i < v.length; i++) {
						vv[i] = erlang2java(v[i]);
					}
					return vv;
				} else {
					return new Object[0];
				}
			} else if (obj instanceof OtpErlangRef) {
				return getTarget((OtpErlangRef) obj);
			} else {
				return obj;
			}
		} catch (Exception e) {
			return obj;
		}
	}

	public static OtpErlangRef registerTarget(Object obj) {
		OtpErlangRef ref = mkref();
		objects.put(ref, obj);
		System.out.println("    >>" + ref + " " + obj);
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
	// TODO we could return several alternatives
	public static Class erlang2javaType(Object obj) throws Exception {
		if (!(obj instanceof OtpErlangObject)) {
			throw new Exception("unsupported erlang2java type");
		}
		if (obj instanceof OtpErlangString) {
			return String.class;
		} else if (obj instanceof OtpErlangInt) {
			return Integer.class;
		} else if (obj instanceof OtpErlangLong) {
			return Long.class;
		} else if (obj instanceof OtpErlangDouble) {
			return Double.class;
		} else if (obj instanceof OtpErlangAtom) {
			OtpErlangAtom a = (OtpErlangAtom) obj;
			if ("true".equals(a.atomValue()) || "false".equals(a.atomValue())) {
				return Boolean.class;
			} else {
				return OtpErlangAtom.class;
			}
		} else if (obj instanceof OtpErlangList) {
			return List.class;
		} else if (obj instanceof OtpErlangTuple) {
			return Object[].class;
		} else if (obj instanceof OtpErlangRef) {
			return objects.get(obj).getClass();
		} else {
			return obj.getClass();
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
				final OtpErlangObject receiver = t.elementAt(1);
				final OtpErlangObject target = t.elementAt(2);
				if ("call".equals(kind.atomValue())) {
					final OtpErlangList args = buildArgs(t.elementAt(3));
					final OtpErlangPid from = (OtpErlangPid) t.elementAt(4);

					rpcHandler.executeRpc(new IRpcExecuter() {
						public void execute() {
							OtpErlangObject result = RpcUtil.execute(receiver,
									target, args.elements());
							rpcHandler.rpcReply(from, result);
						}
					});

				} else if ("uicall".equals(kind.atomValue())) {
					final OtpErlangPid from = (OtpErlangPid) t.elementAt(1);
					final OtpErlangList args = buildArgs(t.elementAt(4));

					// TODO how to mark this as executable in UI thread?
					rpcHandler.executeRpc(new IRpcExecuter() {
						public void execute() {
							OtpErlangObject result = RpcUtil.execute(receiver,
									target, args.elements());
							rpcHandler.rpcReply(from, result);
						}
					});

				} else if ("cast".equals(kind.atomValue())) {
					final OtpErlangList args = buildArgs(t.elementAt(3));

					rpcHandler.executeRpc(new IRpcExecuter() {
						public void execute() {
							RpcUtil.execute(receiver, target, args.elements());
						}
					});

				} else if ("event".equals(kind.atomValue())) {
					String id = ((OtpErlangAtom) receiver).atomValue();

					rpcHandler.rpcEvent(id, target);

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
			OtpErlangObject method, OtpErlangObject[] args) {

		debug("EXEC:: " + target + ":" + method + " " + args + " >"
				+ (args == null ? 0 : args.length));

		MethodDescription description = getDescription(method);

		Object[] parms;
		if (args != null) {
			parms = new Object[args.length];
			for (int i = 0; i < args.length; i++) {
				// parms[i] = args[i];
				// TODO: we can do it better, we have the expected java types in
				// 'description'
				parms[i] = erlang2java(args[i]);
			}
		} else {
			parms = null;
		}

		if (target instanceof OtpErlangRef) {
			// object call
			Object rcvr = getTarget((OtpErlangRef) target);
			if (rcvr != null) {
				try {
					return callMethod(rcvr, description, parms);
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

		} else if (target instanceof OtpErlangAtom
				|| target instanceof OtpErlangString
				|| target instanceof OtpErlangBinary) {
			// static call
			String clazzName = getString(target);
			try {
				Class clazz = Class.forName(clazzName, true, loader);
				return callMethod(clazz, description, parms);
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

	private static MethodDescription getDescription(OtpErlangObject target) {
		System.out.println("@ descr::" + target);
		if (!(target instanceof OtpErlangTuple)) {
			target = new OtpErlangTuple(new OtpErlangObject[] { target,
					new OtpErlangList() });
		}
		OtpErlangTuple t = (OtpErlangTuple) target;
		String name = getString(t.elementAt(0));
		Object olist = erlang2java(t.elementAt(1));
		List<String> arglist = (List<String>) olist;
		Class<?>[] args = new Class<?>[arglist.size()];
		for (int i = 0; i < args.length; i++) {
			try {
				args[i] = Class.forName(arglist.get(i));
			} catch (ClassNotFoundException e) {
				args[i] = Object.class;
			}
		}
		return new MethodDescription(name, args);
	}

	private static String getString(OtpErlangObject target) {
		if (target instanceof OtpErlangAtom) {
			return ((OtpErlangAtom) target).atomValue();
		} else if (target instanceof OtpErlangString) {
			return ((OtpErlangString) target).stringValue();
		} else if (target instanceof OtpErlangBinary) {
			return new String(((OtpErlangBinary) target).binaryValue());
		} else {
			return target.toString();
		}
	}

	private static OtpErlangObject callMethod(Object rcvr,
			MethodDescription method, Object[] args) throws Exception {
		Class cls = (rcvr instanceof Class) ? (Class) rcvr : rcvr.getClass();

		Class[] params = null;
		if (args == null) {
			args = new Object[] {};
		}
		params = new Class[args.length];
		for (int i = 0; i < args.length; i++) {
			params[i] = args[i].getClass();
		}

		Method meth;
		try {
			meth = cls.getMethod(method.name, method.argTypes);
			// meth.setAccessible(true);
			Object o = meth.invoke(rcvr, args);
			debug(String.format("** %s() returned %s", meth, o));

			return java2erlang(o);
		} catch (NoSuchMethodException e) {
			return new OtpErlangTuple(new OtpErlangObject[] {
					new OtpErlangAtom("error"),
					new OtpErlangString(String.format(
							"can't find method %s of %s", method.name, cls
									.getName())) });
		} catch (InvocationTargetException x) {
			Throwable cause = x.getCause();
			log(String.format("invocation of %s failed: %s", method.name, cause
					.getMessage()));
			return new OtpErlangTuple(new OtpErlangObject[] {
					new OtpErlangAtom("error"),
					new OtpErlangString(String.format(
							"invocation of %s failed: %s", method.name, cause
									.getMessage())) });
		}
	}

	private static void log(String s) {
		System.out.println("RpcUtil: " + s);
	}

	private static void debug(String s) {
		if (VERBOSE) {
			log(s);
		}
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

	public static String testing_s(String arg1) {
		return arg1 + arg1;
	}

	public static Object testing(Object arg1, Object arg2) {
		return arg1;
	}

}
