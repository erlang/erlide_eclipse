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
import java.lang.reflect.Method;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.erlide.jinterface.JInterfaceFactory;

import com.ericsson.otp.erlang.OtpErlangAtom;
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

/**
 * Helps converting Java values to Erlang terms, and back. The type information
 * is provided through a string signature, as below.
 * 
 * <dl>
 * <dt>x</dt>
 * <dd>Uses simple conversion, complex types are expected to be OtpErlangObjecs
 * already.</dd>
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
 * <dd>java reference (a distinguished reference, to be used with e->j rpcs)</dd>
 * <dt>l*</dt>
 * <dd>list, the next type descriptor specifies the items' type</dd>
 * <dt>f</dt>
 * <dd>fun -- currently not implemented</dd>
 * <dt>o</dt>
 * <dd>boolean (the atoms true/false)</dd>
 * <dt>0-9</dt>
 * <dd>tuple, the number is the arity and the types of the elements follow in
 * order. Only arities between 0 and 9 are supported.</dd>
 * </dl>
 * 
 */
public class RpcConverter {

	public static Class<?> getClassByName(final String arg) {
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
			return Class.forName(arg, true, JRpcUtil.loader);
		} catch (final ClassNotFoundException e) {
			System.out.println("Rpc TypeConverter: can't find class " + arg);
			return Object.class;
		}
	}

	public static Class<?> javaType2erlang(final Class<?> obj) {
		if (obj.isArray()) {
			return OtpErlangTuple.class;
		}
		if (Collection.class.isAssignableFrom(obj)) {
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
	public static Object erlang2java(final OtpErlangObject obj,
			final Class<?> cls) throws RpcException {
		try {
			if (cls == obj.getClass()) {
				return obj;
			}
			// if the conversion method exists, use it
			try {
				final Method method = cls.getMethod("fromErlangObject",
						new Class<?>[] { OtpErlangObject.class });
				method.setAccessible(true);
				final Object o = method.invoke(null, obj);
				return o;
			} catch (final NoSuchMethodException e) {
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
					final Object arr = Array.newInstance(
							cls.getComponentType(), els.length);
					for (int i = 0; i < els.length; i++) {
						Array.set(arr, i, erlang2java(els[i], cls
								.getComponentType()));
					}
					return arr;
				}
				if (obj instanceof OtpErlangString) {
					final byte[] s = ((OtpErlangString) obj).stringValue()
							.getBytes();
					final Object arr = Array.newInstance(
							cls.getComponentType(), s.length);

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
					final OtpErlangObject[] els = ((OtpErlangList) obj)
							.elements();
					final StringBuilder res = new StringBuilder();
					for (final OtpErlangObject el : els) {
						if (el instanceof OtpErlangLong) {
							final long l = ((OtpErlangLong) el).longValue();
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
					final long res = ((OtpErlangLong) obj).longValue();
					if (cls == char.class || cls == Character.class) {
						return (char) res;
					}
					if (cls == int.class || cls == Integer.class) {
						return (int) res;
					}
					if (cls == byte.class || cls == Byte.class) {
						return (byte) res;
					}
					if (cls == short.class || cls == Short.class) {
						return (short) res;
					}
					if (cls == long.class || cls == Long.class) {
						return res;
					}
				}
				throw new RpcException("wrong arg type "
						+ obj.getClass().getName() + ", can't convert to "
						+ cls.getCanonicalName());
			}
			if (cls == boolean.class || cls == Boolean.class) {
				if (obj instanceof OtpErlangAtom) {
					final String s = ((OtpErlangAtom) obj).atomValue();
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
			if (Collection.class.isAssignableFrom(cls)) {
				if (obj instanceof OtpErlangList) {
					final OtpErlangObject[] list = ((OtpErlangList) obj)
							.elements();
					final Object[] olist = new Object[list.length];
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
				if (!((OtpErlangRef) obj).node().equals(JRpcUtil.REF_NODE)) {
					return ObjRefCache.getTarget((OtpErlangRef) obj);
				}
				throw new RpcException("wrong arg type "
						+ obj.getClass().getName() + ", can't convert to "
						+ cls.getCanonicalName());
			}
			return obj;
		} catch (final RpcException e) {
			throw e;
		} catch (final Exception e) {
			throw new RpcException(e);
		}
	}

	/**
	 * Converts Java objects to Erlang terms.<br/>
	 * 
	 * @param obj
	 *            the object to be converted
	 * @param type
	 *            the desired result's type
	 * @return
	 * @throws ConversionException
	 */
	public static OtpErlangObject java2erlang(final Object obj,
			final String type) throws RpcException {
		return java2erlang(obj, Signature.parse(type)[0]);
	}

	@SuppressWarnings("boxing")
	public static OtpErlangObject java2erlang(final Object obj,
			final Signature type) throws RpcException {
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
			}
			failConversion(obj, type);
		}
		if (obj instanceof Number) {
			if (obj instanceof Float) {
				if (type.kind == 'd') {
					return new OtpErlangFloat((Float) obj);
				}
				failConversion(obj, type);
			} else if (obj instanceof Double) {
				if (type.kind == 'd') {
					return new OtpErlangDouble((Double) obj);
				}
				failConversion(obj, type);
			} else if (type.kind == 'i') {
				if (obj instanceof BigInteger) {
					return new OtpErlangLong((BigInteger) obj);
				}
				return new OtpErlangLong(((Number) obj).longValue());
			} else {
				failConversion(obj, type);
			}
		}
		if (obj instanceof Boolean) {
			if (type.kind == 'o') {
				return new OtpErlangAtom((Boolean) obj ? "true" : "false");
			}
			failConversion(obj, type);
		}
		if (obj instanceof Collection<?>) {
			if (type.kind == 'l') {
				final Object[] v = ((Collection<?>) obj)
						.toArray(new Object[] {});
				final OtpErlangObject[] vv = new OtpErlangObject[v.length];
				for (int i = 0; i < v.length; i++) {
					vv[i] = java2erlang(v[i], type.content[0]);
				}
				return new OtpErlangList(vv);
			}
			failConversion(obj, type);
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
			if (RpcConverter.isCheckConversion()) {
				StackTraceElement el = null;
				StackTraceElement[] st = null;
				try {
					throw new Exception("");
				} catch (final Exception e) {
					boolean found = false;
					st = e.getStackTrace();
					for (final StackTraceElement ste : st) {
						if (found) {
							if (!((ste.getMethodName().equals("send")
									|| ste.getMethodName().equals("sendRpc")
									|| ste.getMethodName().equals("rpc")
									|| ste.getMethodName().equals("rpct")
									|| ste.getMethodName().equals("rpcx") || ste
									.getMethodName().equals("rpcxt")) && ste
									.getClassName().endsWith("Backend"))) {
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
								&& ste.getClassName().endsWith("Backend")) {
							found = true;

						}
					}
				}
				System.out.println(" *** deprecated use of java2erlang: "
						+ obj.getClass().getSimpleName() + " " + el);
				if (el == null) {
					System.out.println("$$$");
					for (final StackTraceElement ste : st) {
						System.out.println("   " + ste);
					}
				}
			}
			return (OtpErlangObject) obj;
		}
		if (obj instanceof IConvertible) {
			return ((IConvertible) obj).toErlangObject();
		}

		if (obj != null && obj.getClass().isArray()) {
			final int len = Array.getLength(obj);
			// Class<?> component = obj.getClass().getComponentType();
			if (type.kind == 'b') {
				// TODO
				return new OtpErlangBinary(obj);
			}
			if (type.kind == 'l') {
				final OtpErlangObject[] vv = new OtpErlangObject[len];
				for (int i = 0; i < len; i++) {
					vv[i] = java2erlang(Array.get(obj, i), type.content[0]);
				}
				return new OtpErlangList(vv);
			} else if (type.kind == 't') {
				final OtpErlangObject[] vv = new OtpErlangObject[len];
				for (int i = 0; i < len; i++) {
					vv[i] = java2erlang(Array.get(obj, i), type.content[i]);
				}
				return new OtpErlangTuple(vv);
			} else {
				failConversion(obj, type);
			}
		}
		if ("j".equals(type)) {
			return ObjRefCache.registerTarget(obj);
		}
		failConversion(obj, type);
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
	private static OtpErlangObject java2erlang(final Object obj) {
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
			return new OtpErlangLong((BigInteger) obj);
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
		if (obj instanceof Collection<?>) {
			final Object[] v = ((Collection<?>) obj).toArray(new Object[] {});
			final OtpErlangObject[] vv = new OtpErlangObject[v.length];
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
		if (obj instanceof OtpErlangString) {
			return (OtpErlangObject) obj;
		}
		if (obj instanceof OtpErlangObject) {
			if (RpcConverter.isCheckConversion()) {
				StackTraceElement el = null;
				StackTraceElement[] st = null;
				try {
					throw new Exception("");
				} catch (final Exception e) {
					boolean found = false;
					st = e.getStackTrace();
					for (final StackTraceElement ste : st) {
						if (found) {
							if (!((ste.getMethodName().equals("send")
									|| ste.getMethodName().equals("rpc")
									|| ste.getMethodName().equals("rpct")
									|| ste.getMethodName().equals("rpcx") || ste
									.getMethodName().equals("rpcxt")) && ste
									.getClassName().endsWith("Backend"))) {
								el = ste;
								break;
							}
						}
						if ((ste.getMethodName().equals("send")
								|| ste.getMethodName().equals("rpc")
								|| ste.getMethodName().equals("rpct")
								|| ste.getMethodName().equals("rpcx") || ste
								.getMethodName().equals("rpcxt"))
								&& ste.getClassName().endsWith(".Backend")) {
							found = true;

						}
					}
				}
				System.out.println(" *** deprecated use of java2erlang: "
						+ obj.getClass().getSimpleName() + " " + el);
				if (el == null) {
					System.out.println("$$$");
					for (final StackTraceElement ste : st) {
						System.out.println("   " + ste);
					}
				}
			}
			return (OtpErlangObject) obj;
		}

		if (obj != null && obj.getClass().isArray()) {
			final int len = Array.getLength(obj);
			final OtpErlangObject[] vv = new OtpErlangObject[len];
			for (int i = 0; i < len; i++) {
				vv[i] = java2erlang(Array.get(obj, i));
			}
			return new OtpErlangList(vv);
		}
		return ObjRefCache.registerTarget(obj);
	}

	private static void failConversion(final Object obj, final Signature type)
			throws RpcException {
		// System.out.println("+++++++ "
		// + String.format("Bad conversion required: %s(%s) - %s", obj
		// .getClass().getName(), obj.toString(), type));

		throw new RpcException(String.format(
				"Bad conversion required: %s(%s) - %s", obj.getClass()
						.getName(), obj.toString(), type.toString()));
	}

	public static boolean isCheckConversion() {
		final String dev = System.getProperty("erlide.test_rpc");
		return dev != null && "true".equals(dev);
	}

	public static boolean matchSignature(final OtpErlangObject term,
			final Signature signature) {
		if (signature.kind == 'x') {
			return true;
		}
		if (term instanceof OtpErlangAtom) {
			return signature.kind == 'a';
		}
		if (term instanceof OtpErlangLong) {
			return signature.kind == 'i';
		}
		if (term instanceof OtpErlangPid) {
			return signature.kind == 'p';
		}
		return false;
	}

	public static boolean matchSignature(final OtpErlangObject term,
			final String signature) throws RpcException {
		return matchSignature(term, Signature.parse(signature)[0]);
	}

	public static OtpErlangObject encodeMap(Map<String, OtpErlangObject> map) {
		final Set<Entry<String, OtpErlangObject>> v = map.entrySet();
		final OtpErlangObject[] vv = new OtpErlangObject[v.size()];
		int i = 0;
		for (Entry<String, OtpErlangObject> entry : v) {
			OtpErlangAtom key = new OtpErlangAtom(entry.getKey());
			OtpErlangObject value = entry.getValue();
			vv[i] = JInterfaceFactory.mkTuple(key, value);
			i++;
		}
		return new OtpErlangList(vv);
	}
}
