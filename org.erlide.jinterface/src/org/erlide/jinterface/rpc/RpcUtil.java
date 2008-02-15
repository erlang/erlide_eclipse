/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.jinterface.rpc;

import java.lang.ref.WeakReference;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class RpcUtil {

	static final String REF_NODE = "jRPC";

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
	private static int refid0 = 0;
	private static int refid1 = 0;
	private static int refid2 = 0;

	// eclipse uses different classloaders for each plugin. this one is non-ui
	// so we have to set it from a ui one (when that one is initialized) so that
	// we can access even the UI classes (which are actually most interesting)
	public static ClassLoader loader = RpcUtil.class.getClassLoader();

	public static OtpErlangTuple buildRpcCall(final String module,
			final String fun, final OtpErlangObject[] args,
			final OtpErlangPid pid) {
		final OtpErlangObject m = new OtpErlangAtom(module);
		final OtpErlangObject f = new OtpErlangAtom(fun);
		final OtpErlangObject a = new OtpErlangList(args);
		return new OtpErlangTuple(pid, new OtpErlangTuple(new OtpErlangAtom(
				"call"), m, f, a, new OtpErlangAtom("user")));
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

	static OtpErlangRef mkref() {
		final int max = 0x7fffffff;
		if (refid2 < max) {
			refid2++;
		} else {
			if (refid1 < max) {
				refid1++;
			} else {
				refid0++;
				refid1 = 0;
			}
			refid2 = 0;
		}
		return new OtpErlangRef(REF_NODE, new int[] { refid0, refid1, refid2 },
				0);
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

					rpcHandler.executeRpc(new Runnable() {
						public void run() {
							OtpErlangObject result = RpcUtil.execute(receiver,
									target, args.elements());
							rpcHandler.rpcReply(from, result);
						}
					});

				} else if ("uicall".equals(kind.atomValue())) {
					final OtpErlangPid from = (OtpErlangPid) t.elementAt(1);
					final OtpErlangList args = buildArgs(t.elementAt(4));

					// TODO how to mark this as executable in UI thread?
					rpcHandler.executeRpc(new Runnable() {
						public void run() {
							OtpErlangObject result = RpcUtil.execute(receiver,
									target, args.elements());
							rpcHandler.rpcReply(from, result);
						}
					});

				} else if ("cast".equals(kind.atomValue())) {
					final OtpErlangList args = buildArgs(t.elementAt(3));

					rpcHandler.executeRpc(new Runnable() {
						public void run() {
							RpcUtil.execute(receiver, target, args.elements());
						}
					});

				} else if ("event".equals(kind.atomValue())) {
					final String id = ((OtpErlangAtom) receiver).atomValue();

					// rpcHandler.rpcEvent(id, target);
					rpcHandler.executeRpc(new Runnable() {
						public void run() {
							rpcHandler.rpcEvent(id, target);

						}
					});

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

	static OtpErlangObject execute(OtpErlangObject target,
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
					parms[i] = RpcConverter.erlang2java(args[i],
							description.argTypes[i]);
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
					return new OtpErlangTuple(new OtpErlangAtom("error"),
							new OtpErlangString(String.format("Bad RPC: %s", e
									.getMessage())));
				}

			}
			log("RPC: unknown receiver: " + target);
			return new OtpErlangTuple(new OtpErlangAtom("error"),
					new OtpErlangString(String.format(
							"Bad RPC: unknown object ref %s%n", target)));

		} else if (target instanceof OtpErlangAtom
				|| target instanceof OtpErlangString
				|| target instanceof OtpErlangBinary) {
			// static call
			String clazzName = getString(target);
			try {
				Class<?> clazz = Class.forName(clazzName, true, loader);
				return callMethod(clazz, description, parms);
			} catch (Exception e) {
				log("bad RPC 2: " + e.getClass() + " " + e.getMessage());
				e.printStackTrace();
				return new OtpErlangTuple(new OtpErlangAtom("error"),
						new OtpErlangString(String.format("Bad RPC: %s", e
								.getMessage())));
			}
		} else {
			log("unknown receiver: " + target);
			return new OtpErlangTuple(new OtpErlangAtom("error"),
					new OtpErlangString(String.format(
							"Bad RPC: unknown receiver %s", target)));
		}
	}

	@SuppressWarnings("unchecked")
	private static MethodDescription getDescription(OtpErlangObject target) {
		if (!(target instanceof OtpErlangTuple)) {
			target = new OtpErlangTuple(target, new OtpErlangList());
		}
		OtpErlangTuple t = (OtpErlangTuple) target;
		String name = getString(t.elementAt(0));
		Object olist = null;
		try {
			olist = RpcConverter.erlang2java(t.elementAt(1), String[].class);
		} catch (RpcException e) {
			// can't fail for String
		}
		if (olist instanceof List) {
			List<String> arglist = (List<String>) olist;
			Class<?>[] args = new Class<?>[arglist.size()];
			for (int i = 0; i < args.length; i++) {
				String arg = arglist.get(i);
				args[i] = RpcConverter.getClassByName(arg);
			}
			return new MethodDescription(name, args);
		} else if (olist instanceof Object[]) {
			Object[] arglist = (Object[]) olist;
			Class<?>[] args = new Class<?>[arglist.length];
			for (int i = 0; i < args.length; i++) {
				String arg = (String) arglist[i];
				args[i] = RpcConverter.getClassByName(arg);
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
		Class<?> cls = (rcvr instanceof Class) ? (Class<?>) rcvr : rcvr
				.getClass();

		Class<?>[] params = null;
		if (args == null) {
			args = new Object[] {};
		}
		params = new Class[args.length];
		for (int i = 0; i < args.length; i++) {
			params[i] = args[i].getClass();
		}

		try {
			if (method.name.equals(cls.getName())) {
				Constructor<?> ctr;
				ctr = cls.getConstructor(method.argTypes);
				// meth.setAccessible(true);
				Object o = ctr.newInstance(args);
				debug(String.format("** %s() returned %s", ctr, o));

				return RpcConverter.java2erlang(o);
			}
			Method meth;
			meth = cls.getMethod(method.name, method.argTypes);
			// meth.setAccessible(true);
			Object o = meth.invoke(rcvr, args);
			debug(String.format("** %s() returned %s", meth, o));

			return RpcConverter.java2erlang(o);
		} catch (NoSuchMethodException e) {
			StringBuilder paramstr = new StringBuilder();
			for (Class<?> param : params) {
				paramstr.append(param.getName()).append(",");
			}
			return new OtpErlangTuple(new OtpErlangAtom("error"),
					new OtpErlangString(String.format(
							"can't find method %s of %s(%s)", method.name, cls
									.getName(), paramstr)));
		} catch (InvocationTargetException x) {
			Throwable cause = x.getCause();
			log(String.format("invocation of %s failed: %s", method.name, cause
					.getMessage()));
			return new OtpErlangTuple(new OtpErlangAtom("error"),
					new OtpErlangString(String.format(
							"invocation of %s failed: %s", method.name, cause
									.getMessage())));
		} catch (IllegalArgumentException x) {
			StringBuilder paramstr = new StringBuilder();
			for (Class<?> param : params) {
				paramstr.append(param.getName()).append(",");
			}
			log(String.format("invocation of %s failed: %s -- %s", method.name,
					x.getMessage(), paramstr));
			return new OtpErlangTuple(new OtpErlangAtom("error"),
					new OtpErlangString(String.format(
							"invocation of %s failed: %s", method.name, x
									.getMessage())));
		} catch (InstantiationException e) {
			StringBuilder paramstr = new StringBuilder();
			for (Class<?> param : params) {
				paramstr.append(param.getName()).append(",");
			}
			log(String.format("instantiation of %s failed: %s -- %s", cls
					.getName(), e.getMessage(), paramstr));
			return new OtpErlangTuple(new OtpErlangAtom("error"),
					new OtpErlangString(String.format(
							"invocation of %s failed: %s", cls.getName(), e
									.getMessage())));
		} catch (IllegalAccessException e) {
			e.printStackTrace();
			return null;
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

}
