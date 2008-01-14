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
				if (obj instanceof OtpErlangList) {
					OtpErlangObject[] els = ((OtpErlangList) obj).elements();
					StringBuffer res = new StringBuffer();
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
					return RpcUtil.getTarget((OtpErlangRef) obj);
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

	@SuppressWarnings( { "boxing", "null" })
	public static OtpErlangObject java2erlang(Object obj) {
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
		return RpcUtil.registerTarget(obj);
	}

	public static boolean isDeveloper() {
		final String dev = System.getProperty("erlide.devel");
		return dev != null && "true".equals(dev);
	}

}
