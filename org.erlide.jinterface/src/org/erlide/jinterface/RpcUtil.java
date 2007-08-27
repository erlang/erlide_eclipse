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

import java.lang.ref.WeakReference;
import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBigLong;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangByte;
import com.ericsson.otp.erlang.OtpErlangChar;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangFloat;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangShort;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class RpcUtil {

	private static final String REF_NODE = "RPC";

	private static class MethodDescription {
		public MethodDescription(String meth, Class<?>[] args) {
			name = meth;
			argTypes = args;
		}

		String name;
		Class<?>[] argTypes;
	}

	private static final boolean VERBOSE = false;

	private static Map<OtpErlangRef, WeakReference<Object>> objects = new HashMap<OtpErlangRef, WeakReference<Object>>();
	private static int refid = 1;

	// eclipse uses different classloaders for each plugin. this one is non-ui
	// so we have to set it from a ui one (when that one is initialized) so that
	// we can access even the UI classes (which are actually most interesting)
	public static ClassLoader loader = RpcUtil.class.getClassLoader();

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

	public static OtpErlangRef registerTarget(Object obj) {
		if (obj == null) {
			return new OtpErlangRef(REF_NODE, new int[] { 0, 0, 0 }, 0);
		}

		Set<Entry<OtpErlangRef, WeakReference<Object>>> entries = objects
				.entrySet();
		for (Entry<OtpErlangRef, WeakReference<Object>> entry : entries) {
			if (entry.getValue().get() == obj) {
				return entry.getKey();
			}
		}
		OtpErlangRef ref = mkref();
		objects.put(ref, new WeakReference<Object>(obj));
		return ref;
	}

	public static Object getTarget(OtpErlangRef ref) {
		return objects.get(ref).get();
	}

	public static void unregisterTarget(OtpErlangRef ref) {
		objects.remove(ref);
	}

	private static OtpErlangRef mkref() {
		return new OtpErlangRef(REF_NODE,
				new int[] { refid++, refid++, refid++ }, 0);
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
				try {
					parms[i] = erlang2java(args[i], description.argTypes[i]);
				} catch (RpcException e) {
					e.printStackTrace();
				}
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
					log("bad RPC 1: " + e.getMessage());
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
				log("bad RPC 2: " + e.getClass() + " " + e.getMessage());
				e.printStackTrace();
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

	@SuppressWarnings("unchecked")
	private static MethodDescription getDescription(OtpErlangObject target) {
		if (!(target instanceof OtpErlangTuple)) {
			target = new OtpErlangTuple(new OtpErlangObject[] { target,
					new OtpErlangList() });
		}
		OtpErlangTuple t = (OtpErlangTuple) target;
		String name = getString(t.elementAt(0));
		Object olist = null;
		try {
			olist = erlang2java(t.elementAt(1), String[].class);
		} catch (RpcException e) {
			// can't fail for String
		}
		if (olist instanceof List) {
			List<String> arglist = (List<String>) olist;
			Class<?>[] args = new Class<?>[arglist.size()];
			for (int i = 0; i < args.length; i++) {
				String arg = arglist.get(i);
				args[i] = getClassByName(arg);
			}
			return new MethodDescription(name, args);
		} else if (olist instanceof Object[]) {
			Object[] arglist = (Object[]) olist;
			Class<?>[] args = new Class<?>[arglist.length];
			for (int i = 0; i < args.length; i++) {
				String arg = (String) arglist[i];
				args[i] = getClassByName(arg);
			}
			return new MethodDescription(name, args);
		} else {
			// TODO should throw exception
			return null;
		}
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
			MethodDescription method, Object[] args) {
		Class cls = (rcvr instanceof Class) ? (Class) rcvr : rcvr.getClass();

		Class[] params = null;
		if (args == null) {
			args = new Object[] {};
		}
		params = new Class[args.length];
		for (int i = 0; i < args.length; i++) {
			params[i] = args[i].getClass();
		}

		if (method.name.equals(cls.getName())) {
			Constructor ctr;
			try {
				ctr = cls.getConstructor(method.argTypes);
				// meth.setAccessible(true);
				Object o = ctr.newInstance(args);
				debug(String.format("** %s() returned %s", ctr, o));

				return java2erlang(o);
			} catch (NoSuchMethodException e) {
				StringBuffer paramstr = new StringBuffer();
				for (Class param : params) {
					paramstr.append(param.getName()).append(",");
				}
				return new OtpErlangTuple(new OtpErlangObject[] {
						new OtpErlangAtom("error"),
						new OtpErlangString(String.format(
								"can't find method %s of %s(%s)", method.name,
								cls.getName(), paramstr)) });
			} catch (InvocationTargetException x) {
				Throwable cause = x.getCause();
				log(String.format("invocation of %s failed: %s", method.name,
						cause.getMessage()));
				return new OtpErlangTuple(new OtpErlangObject[] {
						new OtpErlangAtom("error"),
						new OtpErlangString(String.format(
								"invocation of %s failed: %s", method.name,
								cause.getMessage())) });
			} catch (IllegalArgumentException x) {
				StringBuffer paramstr = new StringBuffer();
				for (Class param : params) {
					paramstr.append(param.getName()).append(",");
				}
				log(String.format("invocation of %s failed: %s -- %s",
						method.name, x.getMessage(), paramstr));
				return new OtpErlangTuple(new OtpErlangObject[] {
						new OtpErlangAtom("error"),
						new OtpErlangString(String.format(
								"invocation of %s failed: %s", method.name, x
										.getMessage())) });
			} catch (InstantiationException e) {
				StringBuffer paramstr = new StringBuffer();
				for (Class param : params) {
					paramstr.append(param.getName()).append(",");
				}
				log(String.format("instantiation of %s failed: %s -- %s", cls
						.getName(), e.getMessage(), paramstr));
				return new OtpErlangTuple(new OtpErlangObject[] {
						new OtpErlangAtom("error"),
						new OtpErlangString(String.format(
								"invocation of %s failed: %s", cls.getName(), e
										.getMessage())) });
			} catch (IllegalAccessException e) {
				e.printStackTrace();
				return null;
			}

		} else {
			Method meth;
			try {
				meth = cls.getMethod(method.name, method.argTypes);
				// meth.setAccessible(true);
				Object o = meth.invoke(rcvr, args);
				debug(String.format("** %s() returned %s", meth, o));

				return java2erlang(o);
			} catch (NoSuchMethodException e) {
				StringBuffer paramstr = new StringBuffer();
				for (Class param : params) {
					paramstr.append(param.getName()).append(",");
				}
				return new OtpErlangTuple(new OtpErlangObject[] {
						new OtpErlangAtom("error"),
						new OtpErlangString(String.format(
								"can't find method %s of %s(%s)", method.name,
								cls.getName(), paramstr)) });
			} catch (InvocationTargetException x) {
				Throwable cause = x.getCause();
				log(String.format("invocation1 of %s failed: %s", method.name,
						cause.getMessage()));
				return new OtpErlangTuple(new OtpErlangObject[] {
						new OtpErlangAtom("error"),
						new OtpErlangString(String.format(
								"invocation1 of %s failed: %s", method.name,
								cause.getMessage())) });
			} catch (IllegalArgumentException x) {
				StringBuffer paramstr = new StringBuffer();
				for (Class param : params) {
					paramstr.append(param.getName()).append(",");
				}
				log(String.format("invocation2 of %s failed: %s -- %s",
						method.name, x.getMessage(), paramstr));
				return new OtpErlangTuple(new OtpErlangObject[] {
						new OtpErlangAtom("error"),
						new OtpErlangString(String.format(
								"invocation2 of %s failed: %s", method.name, x
										.getMessage())) });
			} catch (IllegalAccessException e) {
				e.printStackTrace();
				return null;
			}
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

	@SuppressWarnings( { "boxing", "null" })
	public static OtpErlangObject java2erlang(Object obj) {
		if (obj instanceof String) {
			return new OtpErlangString((String) obj);
		}
		if (obj instanceof Character) {
			return new OtpErlangChar((Character) obj);
		}
		// if (char.class.isInstance(obj)) {
		// char c = (char) obj;
		// return new OtpErlangChar(c);
		// }
		if (obj instanceof Byte) {
			return new OtpErlangByte((Byte) obj);
		}
		// if (byte.class.isInstance(obj)) {
		// return new OtpErlangByte(((Byte) obj).byteValue());
		// }
		if (obj instanceof Short) {
			return new OtpErlangShort((Short) obj);
		}
		// if (short.class.isInstance(obj)) {
		// return new OtpErlangShort((Short) obj);
		// }
		if (obj instanceof Integer) {
			return new OtpErlangInt((Integer) obj);
		}
		// if (int.class.isInstance(obj)) {
		// return new OtpErlangInt((Integer) obj);
		// }
		if (obj instanceof Long) {
			return new OtpErlangLong((Long) obj);
		}
		// if (long.class.isInstance(obj)) {
		// return new OtpErlangLong((Long) obj);
		// }
		if (obj instanceof BigInteger) {
			return new OtpErlangBigLong((BigInteger) obj);
		}
		if (obj instanceof Float) {
			return new OtpErlangFloat((Float) obj);
		}
		// if (float.class.isInstance(obj)) {
		// return new OtpErlangFloat((Float) obj);
		// }
		if (obj instanceof Double) {
			return new OtpErlangDouble((Double) obj);
		}
		// if (double.class.isInstance(obj)) {
		// return new OtpErlangDouble((Double) obj);
		// }
		if (obj instanceof Boolean) {
			return new OtpErlangAtom((Boolean) obj ? "true" : "false");
		}
		// if (boolean.class.isInstance(obj)) {
		// return new OtpErlangAtom((Boolean) obj ? "true" : "false");
		// }
		if (obj instanceof List<?>) {
			Object[] v = ((List<?>) obj).toArray(new Object[] {});
			OtpErlangObject[] vv = new OtpErlangObject[v.length];
			for (int i = 0; i < v.length; i++) {
				vv[i] = java2erlang(v[i]);
			}
			return new OtpErlangList(vv);
		}

		if (obj instanceof OtpErlangPid) {
			return (OtpErlangPid) obj;
		}
		if (obj instanceof OtpErlangRef) {
			return (OtpErlangObject) obj;
		}
		if (obj instanceof OtpErlangTuple) {
			return (OtpErlangObject) obj;
		}
		if (obj instanceof OtpErlangAtom) {
			return (OtpErlangObject) obj;
		}
		if (obj instanceof OtpErlangBinary) {
			return (OtpErlangObject) obj;
		}
		if (obj instanceof OtpErlangObject) {
			StackTraceElement el = null;
			StackTraceElement[] st = null;
			try {
				throw new Exception("");
			} catch (Exception e) {
				boolean found = false;
				st = e.getStackTrace();
				for (StackTraceElement ste : st) {
					if (found) {
						if (!((ste.getMethodName().equals("send")
								|| ste.getMethodName().equals("rpc") || ste
								.getMethodName().equals("rpct")) && ste
								.getClassName().endsWith("AbstractBackend"))) {
							el = ste;
							break;
						}
					}
					if ((ste.getMethodName().equals("send")
							|| ste.getMethodName().equals("rpc") || ste
							.getMethodName().equals("rpct"))
							&& ste.getClassName().endsWith("AbstractBackend")) {
						found = true;

					}
				}
			}
			if (isDeveloper()) {
				System.out.println(" *** deprecated use of java2erlang: "
						+ obj.getClass().getSimpleName() + " " + el);
				if (el == null) {
					System.out.println("$$$");
					for (StackTraceElement ste : st) {
						System.out.println("   " + ste);
					}
				}
			}
			return (OtpErlangObject) obj;
		}

		if (obj != null && obj.getClass().isArray()) {
			Class klass = obj.getClass().getComponentType();
			int len = Array.getLength(obj);
			OtpErlangObject[] vv = new OtpErlangObject[len];
			for (int i = 0; i < len; i++) {
				vv[i] = java2erlang(Array.get(obj, i));
			}
			return new OtpErlangList(vv);
		}
		return registerTarget(obj);
	}

	@SuppressWarnings("boxing")
	public static Object erlang2java(OtpErlangObject obj, Class cls)
			throws RpcException {
		try {
			if (cls == obj.getClass()) {
				return obj;
			}

			if (cls.isArray()) {
				OtpErlangObject[] els = null;
				if (obj instanceof OtpErlangList) {
					els = ((OtpErlangList) obj).elements();
				}
				if (obj instanceof OtpErlangTuple) {
					els = ((OtpErlangTuple) obj).elements();
				}
				if (els != null) {
					Object arr = Array.newInstance(cls.getComponentType(),
							els.length);
					for (int i = 0; i < els.length; i++) {
						Array.set(arr, i, erlang2java(els[i], cls
								.getComponentType()));
					}
					return arr;
				} else {
					if (obj instanceof OtpErlangString) {
						byte[] s = ((OtpErlangString) obj).stringValue()
								.getBytes();
						Object arr = Array.newInstance(cls.getComponentType(),
								s.length);

						for (int i = 0; i < s.length; i++) {
							Array.set(arr, i, s[i]);
						}
						return arr;
					}
					return new Object[0];
				}
			}

			if (cls == String.class) {
				if (obj instanceof OtpErlangString) {
					return ((OtpErlangString) obj).stringValue();
				}
				if (obj instanceof OtpErlangAtom) {
					return ((OtpErlangAtom) obj).atomValue();
				}
				if (obj instanceof OtpErlangBinary) {
					return new String(((OtpErlangBinary) obj).binaryValue());
				}
				// if (obj instanceof OtpErlangList) {
				// // TODO check if string and convert
				// return null;
				// }
				throw new RpcException("wrong arg type "
						+ obj.getClass().getName()
						+ ", can't convert to String");
			}
			if (cls == char.class || cls == Character.class || cls == int.class
					|| cls == Integer.class || cls == byte.class
					|| cls == Byte.class || cls == short.class
					|| cls == Short.class || cls == long.class
					|| cls == Long.class) {
				if (obj instanceof OtpErlangLong) {
					long res = ((OtpErlangLong) obj).longValue();
					if (cls == char.class) {
						return (char) res;
					}
					if (cls == int.class) {
						return (int) res;
					}
					if (cls == byte.class) {
						return (byte) res;
					}
					if (cls == short.class) {
						return (short) res;
					}
					if (cls == long.class) {
						return res;
					}
				}
				return null;
			}
			if (cls == boolean.class || cls == Boolean.class) {
				if (obj instanceof OtpErlangAtom) {
					String s = ((OtpErlangAtom) obj).atomValue();
					if (s.equals("true")) {
						return true;
					}
					if (s.equals("false")) {
						return false;
					}
				}
			}
			if (List.class.isAssignableFrom(cls)) {
				if (obj instanceof OtpErlangList) {
					OtpErlangObject[] list = ((OtpErlangList) obj).elements();
					Object[] olist = new Object[list.length];
					for (int i = 0; i < list.length; i++) {
						olist[i] = erlang2java(list[i], list[i].getClass());
					}
					return Arrays.asList(olist);
				}
			}
			if (obj instanceof OtpErlangRef) {
				if (!((OtpErlangRef) obj).node().equals(REF_NODE)) {
					return getTarget((OtpErlangRef) obj);
				}
			}
			if (obj instanceof OtpErlangObject) {
				return obj;
			}

			return obj;
		} catch (Exception e) {
			throw new RpcException(e);
		}
	}

	private static Class getClassByName(String arg) {
		if (arg.equals("char")) {
			return char.class;
		}
		if (arg.equals("byte")) {
			return byte.class;
		}
		if (arg.equals("short")) {
			return short.class;
		}
		if (arg.equals("int")) {
			return int.class;
		}
		if (arg.equals("long")) {
			return long.class;
		}
		if (arg.equals("boolean")) {
			return boolean.class;
		}
		if (arg.equals("float")) {
			return float.class;
		}
		if (arg.equals("double")) {
			return double.class;
		}
		try {
			return Class.forName(arg, true, loader);
		} catch (ClassNotFoundException e) {
			System.out.println("RpcUtil: can't find class " + arg);
			return Object.class;
		}

	}

	public static boolean isDeveloper() {
		final String dev = System.getProperty("erlide.devel");
		return dev != null && "true".equals(dev);
	}

}
