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

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.erlide.jinterface.JInterfaceFactory;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class JRpcUtil {
	static final String REF_NODE = "jRPC";

	// eclipse uses different classloaders for each plugin. this one is non-ui
	// so we have to set it from a ui one (when that one is initialized) so that
	// we can access even the UI classes (which are actually most interesting)
	public static ClassLoader loader = JRpcUtil.class.getClassLoader();

	private static final boolean VERBOSE = false;

	public static OtpErlangObject execute(OtpErlangObject target,
			OtpErlangObject method, OtpErlangObject[] args) {

		debug("EXEC:: " + target + ":" + method + " " + args + " >"
				+ (args == null ? 0 : args.length));

		MethodDescription description = getDescription(method);

		Object[] parms;
		if (args != null) {
			parms = new Object[args.length];
			for (int i = 0; i < args.length; i++) {
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
			Object rcvr = ObjRefCache.getTarget((OtpErlangRef) target);
			if (rcvr != null) {
				try {
					return callMethod(rcvr, description, parms);
				} catch (Exception e) {
					log("bad RPC 1: " + e.getMessage());
					return JInterfaceFactory.mkTuple(
							new OtpErlangAtom("error"), new OtpErlangString(
									String
											.format("Bad RPC: %s", e
													.getMessage())));
				}

			}
			log("RPC: unknown receiver: " + target);
			return JInterfaceFactory.mkTuple(new OtpErlangAtom("error"),
					new OtpErlangString(String.format(
							"Bad RPC: unknown object ref %s%n", target)));

		} else if (target instanceof OtpErlangAtom
				|| target instanceof OtpErlangString
				|| target instanceof OtpErlangBinary) {
			// static call
			String clazzName = asString(target);
			try {
				Class<?> clazz = Class.forName(clazzName, true, loader);
				return callMethod(clazz, description, parms);
			} catch (Exception e) {
				log("bad RPC 2: " + e.getClass() + " " + e.getMessage());
				e.printStackTrace();
				return JInterfaceFactory.mkTuple(new OtpErlangAtom("error"),
						new OtpErlangString(String.format("Bad RPC: %s", e
								.getMessage())));
			}
		} else {
			log("unknown receiver: " + target);
			return JInterfaceFactory.mkTuple(new OtpErlangAtom("error"),
					new OtpErlangString(String.format(
							"Bad RPC: unknown receiver %s", target)));
		}
	}

	@SuppressWarnings("unchecked")
	private static MethodDescription getDescription(OtpErlangObject target) {
		if (!(target instanceof OtpErlangTuple)) {
			target = JInterfaceFactory.mkTuple(target, new OtpErlangList());
		}
		OtpErlangTuple t = (OtpErlangTuple) target;
		String name = asString(t.elementAt(0));
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

	private static String asString(OtpErlangObject target) {
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
		Class<?> cls = (rcvr instanceof Class<?>) ? (Class<?>) rcvr : rcvr
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

				return RpcConverter.java2erlang(o, "x");
			}
			Method meth;
			meth = cls.getMethod(method.name, method.argTypes);
			// meth.setAccessible(true);
			Object o = meth.invoke(rcvr, args);
			debug(String.format("** %s() returned %s", meth, o));

			return RpcConverter.java2erlang(o, "x");
		} catch (NoSuchMethodException e) {
			StringBuilder paramstr = new StringBuilder();
			for (Class<?> param : params) {
				paramstr.append(param.getName()).append(",");
			}
			return JInterfaceFactory.mkTuple(new OtpErlangAtom("error"),
					new OtpErlangString(String.format(
							"can't find method %s of %s(%s)", method.name, cls
									.getName(), paramstr)));
		} catch (InvocationTargetException x) {
			Throwable cause = x.getCause();
			log(String.format("invocation of %s failed: %s", method.name, cause
					.getMessage()));
			return JInterfaceFactory.mkTuple(new OtpErlangAtom("error"),
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
			return JInterfaceFactory.mkTuple(new OtpErlangAtom("error"),
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
			return JInterfaceFactory.mkTuple(new OtpErlangAtom("error"),
					new OtpErlangString(String.format(
							"invocation of %s failed: %s", cls.getName(), e
									.getMessage())));
		} catch (IllegalAccessException e) {
			e.printStackTrace();
			return null;
		} catch (RpcException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return null;
		}

	}

	/**
	 * This is a generic alternative to callMethod. Tests if an argument is
	 * assignable to the declared method's type. It isn't yet adapted to
	 * java-rpc.<br/>
	 * Based on the paper at
	 * http://www.jgroups.org/javagroupsnew/docs/papers/MethodResolution.ps.gz
	 * 
	 * @param message
	 * @param target
	 * @param args
	 * @return
	 */
	@SuppressWarnings( { "unused", "unchecked" })
	private static Object sendMessage(String message, Object target,
			Object[] args) {
		try {
			// Is this an argumentless method call?
			if (args == null) {
				// Get the method.
				return target.getClass().getMethod(message, (Class[]) null)
						.invoke(target, (Object[]) null);
			}
			// Get all methods from the target.
			Method[] allMethods = target.getClass().getMethods();
			List<Method> candidateMethods = new ArrayList<Method>();

			for (int i = 0; i < allMethods.length; i++) {
				// Filter methods by name and length of arguments.
				Method m = allMethods[i];
				if (m.getName().equals(message)
						&& m.getParameterTypes().length == args.length) {
					candidateMethods.add(m);
				}
			}

			if (candidateMethods.size() == 0) {
				throw new RuntimeException("");
			}

			Method callableMethod = null;
			for (Iterator<Method> itr = candidateMethods.iterator(); itr
					.hasNext();) {
				boolean callable = true;
				Method m = itr.next();
				Class[] argFormalTypes = m.getParameterTypes();
				for (int i = 0; i < argFormalTypes.length; i++) {
					if (!argFormalTypes[i].isAssignableFrom(args[i].getClass())) {
						callable = false;
					}
				}
				if (callable) {
					callableMethod = m;
				}
			}

			if (callableMethod != null) {
				return callableMethod.invoke(target, args);
			}
			throw new RuntimeException("No such method found: " + message);
		} catch (Exception e) {
			StringBuffer sb = new StringBuffer();
			// Build a helpful message to debug reflection issues.
			try {
				sb.append("ERROR: Could not send message '" + message
						+ "' to target of type " + target.getClass().toString()
						+ " \n");

				sb.append("\ttarget implements : \n");
				Class[] interfaces = target.getClass().getInterfaces();
				for (int j = 0; j < interfaces.length; j++) {
					sb.append("\t\t" + interfaces[j].getName() + "\n");
				}
				sb.append("\n");

				sb.append("\ttarget methods: \n");
				Method[] methods = target.getClass().getMethods();
				for (int j = 0; j < methods.length; j++) {
					sb.append("\t\t" + methods[j].getName() + "\n");
				}
				sb.append("\n");

				if (args != null) {
					sb.append("\tArgument types: \n");
					for (int j = 0; j < args.length; j++) {
						sb.append("\t\t" + args[j].getClass().getName() + "\n");
					}
				}
			} catch (Exception e2) {
				throw new RuntimeException(
						"ERROR: Could not create detailed error message for failed sendMessage() call.");
			}
			throw new RuntimeException(sb.toString());
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

	private static void warn(Exception e) {
		log(e.getMessage());
		e.printStackTrace();
	}

	private static class MethodDescription {
		public MethodDescription(String meth, Class<?>[] args) {
			this.name = meth;
			this.argTypes = args;
		}

		String name;
		Class<?>[] argTypes;
	}

	private JRpcUtil() {
	}
}
