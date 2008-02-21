/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.jinterface.rpc;

import java.lang.reflect.Array;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

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

public class RpcConverter {

	public static Class<?> getClassByName(String arg) {
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
			return Class.forName(arg, true, RpcUtil.loader);
		} catch (ClassNotFoundException e) {
			System.out.println("Rpc TypeConverter: can't find class " + arg);
			return Object.class;
		}
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

	@SuppressWarnings("boxing")
	public static Object erlang2java(OtpErlangObject obj, Class<?> cls)
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
				}
				if (obj instanceof OtpErlangString) {
					byte[] s = ((OtpErlangString) obj).stringValue().getBytes();
					Object arr = Array.newInstance(cls.getComponentType(),
							s.length);

					for (int i = 0; i < s.length; i++) {
						Array.set(arr, i, s[i]);
					}
					return arr;
				}
				return new Object[0];
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
				if (obj instanceof OtpErlangList) {
					OtpErlangObject[] els = ((OtpErlangList) obj).elements();
					StringBuilder res = new StringBuilder();
					for (OtpErlangObject el : els) {
						if (el instanceof OtpErlangLong) {
							long l = ((OtpErlangLong) el).longValue();
							res.append((char) (l & 0xFFFF));
						} else {
							res.append(erlang2java(el, String.class));
						}
					}
					return res.toString();
				}
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
				throw new RpcException("wrong arg type "
						+ obj.getClass().getName() + ", can't convert to "
						+ cls.getCanonicalName());
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
				throw new RpcException("wrong arg type "
						+ obj.getClass().getName() + ", can't convert to "
						+ cls.getCanonicalName());
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
				throw new RpcException("wrong arg type "
						+ obj.getClass().getName() + ", can't convert to "
						+ cls.getCanonicalName());
			}
			if (obj instanceof OtpErlangRef) {
				if (!((OtpErlangRef) obj).node().equals(RpcUtil.REF_NODE)) {
					return ObjRefCache.getTarget((OtpErlangRef) obj);
				}
				throw new RpcException("wrong arg type "
						+ obj.getClass().getName() + ", can't convert to "
						+ cls.getCanonicalName());
			}
			return obj;
		} catch (RpcException e) {
			throw e;
		} catch (Exception e) {
			throw new RpcException(e);
		}
	}

	/**
	 * Converts Java objects to Erlang terms.<br/>
	 * <dl>
	 * <dt>x</dt>
	 * <dd>Uses simple conversion, complex types are expected to be
	 * OtpErlangObjecs already.</dd>
	 * <dt>i</dt>
	 * <dd>integer</dd>
	 * <dt>s</dt>
	 * <dd>string</dd>
	 * <dt>a</dt>
	 * <dd>atom</dd>
	 * <dt>d</dt>
	 * <dd>float</dd>
	 * <dt>p</dt>
	 * <dd>pid</dd>
	 * <dt>r</dt>
	 * <dd>reference</dd>
	 * <dt>j</dt>
	 * <dd>java reference (a distinguished reference, to be used with e->j
	 * rpcs)</dd>
	 * <dt>l*</dt>
	 * <dd>list, the next type descriptor specifies the items' type</dd>
	 * <dt>f</dt>
	 * <dd>fun -- currently not implemented</dd>
	 * <dt>o</dt>
	 * <dd>boolean (the atoms true/false)</dd>
	 * <dt>0-9</dt>
	 * <dd>tuple, the number is the arity and the types of the elements follow
	 * in order. Only arities between 0 and 9 are supported.</dd>
	 * </dl>
	 * 
	 * @param obj
	 *            the object to be converted
	 * @param type
	 *            the desired result's type
	 * @return
	 * @throws ConversionException
	 */
	@SuppressWarnings("boxing")
	public static OtpErlangObject java2erlang(Object obj, String type)
			throws RpcException {
		return java2erlang(obj, parseOne(type).sign);
	}

	public static OtpErlangObject java2erlang(Object obj, Signature type)
			throws RpcException {
		if (type.kind == 'x') {
			return java2erlang(obj);
		}
		if (obj instanceof String) {
			if (type.kind == 's') {
				return new OtpErlangString((String) obj);
			} else if (type.kind == 'a') {
				return new OtpErlangAtom((String) obj);
			} else if (type.kind == 'b') {
				return new OtpErlangBinary(((String) obj).getBytes());
			} else {
				failConversion(obj, type);
			}
		}
		if (obj instanceof Character) {
			if (type.kind == 'i') {
				return new OtpErlangChar((Character) obj);
			} else {
				failConversion(obj, type);
			}
		}
		if (obj instanceof Number) {
			if (obj instanceof Float) {
				if (type.kind == 'd') {
					return new OtpErlangFloat((Float) obj);
				} else {
					failConversion(obj, type);
				}
			} else if (obj instanceof Double) {
				if (type.kind == 'd') {
					return new OtpErlangDouble((Double) obj);
				} else {
					failConversion(obj, type);
				}
			} else if (type.kind == 'i') {
				if (obj instanceof BigInteger) {
					return new OtpErlangBigLong((BigInteger) obj);
				} else {
					return new OtpErlangLong(((Number) obj).longValue());
				}
			} else {
				failConversion(obj, type);
			}
		}
		if (obj instanceof Boolean) {
			if (type.kind == 'o') {
				return new OtpErlangAtom(((Boolean) obj) ? "true" : "false");
			} else {
				failConversion(obj, type);
			}
		}
		if (obj instanceof List<?>) {
			if (type.kind == 'l') {
				Object[] v = ((List<?>) obj).toArray(new Object[] {});
				OtpErlangObject[] vv = new OtpErlangObject[v.length];
				for (int i = 0; i < v.length; i++) {
					vv[i] = java2erlang(v[i], type.content[0]);
				}
				return new OtpErlangList(vv);
			} else {
				failConversion(obj, type);
			}
		}

		if (obj instanceof OtpErlangPid) {
			return (OtpErlangPid) obj;
		}
		if (obj instanceof OtpErlangRef) {
			return (OtpErlangObject) obj;
		}
		if (obj instanceof OtpErlangBinary) {
			return (OtpErlangObject) obj;
		}
		if (obj instanceof OtpErlangObject) {
			if (RpcConverter.isDeveloper()) {
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
									|| ste.getMethodName().equals("sendRpc")
									|| ste.getMethodName().equals("rpc")
									|| ste.getMethodName().equals("rpct")
									|| ste.getMethodName().equals("rpcx") || ste
									.getMethodName().equals("rpcxt")) && ste
									.getClassName().endsWith("AbstractBackend"))) {
								el = ste;
								break;
							}
						}
						if ((ste.getMethodName().equals("send")
								|| ste.getMethodName().equals("sendRpc")
								|| ste.getMethodName().equals("rpc")
								|| ste.getMethodName().equals("rpct")
								|| ste.getMethodName().equals("rpcx") || ste
								.getMethodName().equals("rpcxt"))
								&& ste.getClassName().endsWith(
										"AbstractBackend")) {
							found = true;

						}
					}
				}
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
			int len = Array.getLength(obj);
			// Class<?> component = obj.getClass().getComponentType();
			if (type.kind == 'b') {
				// TODO
				return new OtpErlangBinary(obj);
			} else {
				if (type.kind == 'l') {
					OtpErlangObject[] vv = new OtpErlangObject[len];
					for (int i = 0; i < len; i++) {
						vv[i] = java2erlang(Array.get(obj, i), type.content[0]);
					}
					return new OtpErlangList(vv);
				} else if (type.kind == 't') {
					OtpErlangObject[] vv = new OtpErlangObject[len];
					for (int i = 0; i < len; i++) {
						vv[i] = java2erlang(Array.get(obj, i), type.content[i]);
					}
					return new OtpErlangTuple(vv);
				} else {
					failConversion(obj, type);
				}
			}
		}
		if ("j".equals(type)) {
			return ObjRefCache.registerTarget(obj);
		} else {
			failConversion(obj, type);
		}
		return null;
	}

	/**
	 * Old style java->erlang conversion, used when "x" is given as an argument.
	 * TODO Could be polished a little.
	 * 
	 * @param obj
	 * @return
	 */
	@SuppressWarnings("boxing")
	private static OtpErlangObject java2erlang(Object obj) {
		if (obj instanceof String) {
			return new OtpErlangString((String) obj);
		}
		if (obj instanceof Character) {
			return new OtpErlangChar((Character) obj);
		}
		if (obj instanceof Byte) {
			return new OtpErlangByte((Byte) obj);
		}
		if (obj instanceof Short) {
			return new OtpErlangShort((Short) obj);
		}
		if (obj instanceof Integer) {
			return new OtpErlangInt((Integer) obj);
		}
		if (obj instanceof Long) {
			return new OtpErlangLong((Long) obj);
		}
		if (obj instanceof BigInteger) {
			return new OtpErlangBigLong((BigInteger) obj);
		}
		if (obj instanceof Float) {
			return new OtpErlangFloat((Float) obj);
		}
		if (obj instanceof Double) {
			return new OtpErlangDouble((Double) obj);
		}
		if (obj instanceof Boolean) {
			return new OtpErlangAtom((Boolean) obj ? "true" : "false");
		}
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
			if (RpcConverter.isDeveloper()) {
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
									|| ste.getMethodName().equals("rpc")
									|| ste.getMethodName().equals("rpct")
									|| ste.getMethodName().equals("rpcx") || ste
									.getMethodName().equals("rpcxt")) && ste
									.getClassName().endsWith("AbstractBackend"))) {
								el = ste;
								break;
							}
						}
						if ((ste.getMethodName().equals("send")
								|| ste.getMethodName().equals("rpc")
								|| ste.getMethodName().equals("rpct")
								|| ste.getMethodName().equals("rpcx") || ste
								.getMethodName().equals("rpcxt"))
								&& ste.getClassName().endsWith(
										"AbstractBackend")) {
							found = true;

						}
					}
				}
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
			int len = Array.getLength(obj);
			OtpErlangObject[] vv = new OtpErlangObject[len];
			for (int i = 0; i < len; i++) {
				vv[i] = java2erlang(Array.get(obj, i));
			}
			return new OtpErlangList(vv);
		}
		return ObjRefCache.registerTarget(obj);
	}

	private static void failConversion(Object obj, Signature type)
			throws RpcException {
		// System.out.println("+++++++ "
		// + String.format("Bad conversion required: %s(%s) - %s", obj
		// .getClass().getName(), obj.toString(), type));

		throw new RpcException(String.format(
				"Bad conversion required: %s(%s) - %s", obj.getClass()
						.getName(), obj.toString(), type.toString()));
	}

	public static boolean isDeveloper() {
		final String dev = System.getProperty("erlide.test");
		return dev != null && "true".equals(dev);
	}

	public static class Signature {
		public char kind = 'x';
		public Signature[] content = null;

		public Signature(char str) {
			kind = str;
		}

		public Signature(char crt, Signature sub) {
			kind = crt;
			content = new Signature[] { sub };
		}

		public Signature(char crt, Signature[] sub) {
			kind = crt;
			content = sub;
		}

		@Override
		public String toString() {
			String res = "";
			if (content != null) {
				res = "(";
				for (Signature s : content) {
					res += s.toString() + ",";
				}
				res = res.substring(0, res.length() - 1) + ")";
			}
			return kind + res;
		}
	}

	public static Signature[] parseSignature(String signature)
			throws RpcException {
		List<Signature> type = new ArrayList<Signature>();
		if (signature == null) {
			return null;
			// throw new RpcException("Signature is null");
		}
		while (signature.length() > 0) {
			State e = parseOne(signature);
			type.add(e.sign);
			signature = e.rest;
		}
		return type.toArray(new Signature[type.size()]);
	}

	private static class State {
		public State(Signature signature, String substring) {
			sign = signature;
			rest = substring;
		}

		Signature sign;
		String rest;
	}

	private static State parseOne(String signature) throws RpcException {
		char crt = signature.charAt(0);
		if ("xidabrjfpso".indexOf(crt) >= 0) {
			return new State(new Signature(crt), signature.substring(1));
		} else if (crt == 'l') {
			State sub = parseOne(signature.substring(1));
			return new State(new Signature(crt, sub.sign), sub.rest);
		} else if ("0123456789".indexOf(crt) >= 0) {
			int n = Integer.parseInt(signature.substring(0, 1));
			Signature[] sub = new Signature[n];
			String s = signature.substring(1);
			for (int i = 0; i < n; i++) {
				State state = parseOne(s);
				sub[i] = state.sign;
				s = state.rest;
			}
			return new State(new Signature('t', sub), s);
		} else {
			throw new RpcException("unknown signature code: " + crt);
		}
	}
}
