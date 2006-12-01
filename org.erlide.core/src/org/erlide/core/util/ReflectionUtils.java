/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.util;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangChar;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
//import java.lang.reflect.Constructor;
//import java.lang.reflect.Field;
//import java.lang.reflect.Method;

public class ReflectionUtils {

	private ReflectionUtils() {
	}

	public static void exportClass(Class clazz) {
		/* TODO: Define exportClass */
		/*
		final Constructor[] constructors = clazz.getConstructors();
		for (int i = 0; i < constructors.length; i++) {
			final Constructor constructor = constructors[i];

		}
		final Field[] fields = clazz.getFields();
		for (int i = 0; i < fields.length; i++) {
			final Field field = fields[i];

		}
		final Method[] methods = clazz.getMethods();
		for (int i = 0; i < methods.length; i++) {
			final Method method = methods[i];

		}
		*/
	}

	public static Call parseCallMsg(OtpErlangObject msg) {
		try {
			final Call result = new Call();

			final OtpErlangTuple tmsg = (OtpErlangTuple) msg;
			OtpErlangLong objid;
			OtpErlangAtom clazz;
			if (tmsg.elementAt(0) instanceof OtpErlangLong) {
				objid = (OtpErlangLong) tmsg.elementAt(0);
				clazz = null;
			} else {
				clazz = (OtpErlangAtom) tmsg.elementAt(0);
				objid = new OtpErlangLong(0);
			}
			final OtpErlangString meth = (OtpErlangString) tmsg.elementAt(1);
			final OtpErlangList args = (OtpErlangList) tmsg.elementAt(2);

			result.objid = objid.longValue();
			result.meth = meth.stringValue();
			if (clazz != null) {
				final Class cls = Class.forName(clazz.atomValue());
				result.args = parseArgs(cls, meth.stringValue(), args);
			}

			return result;

		} catch (final Exception e) {
			return null;
		}
	}

	private static Object[] parseArgs(Class cls, String string,
			OtpErlangList args) {
		final Object[] result = new Object[args.arity()];
		return result;
	}

	public static Class mapType(Class c) {
		if (c == int.class) {
			return OtpErlangInt.class;
		}
		if (c == OtpErlangInt.class) {
			return int.class;
		}
		if (c == long.class) {
			return OtpErlangLong.class;
		}
		if (c == OtpErlangLong.class) {
			return long.class;
		}
		if (c == String.class) {
			return OtpErlangString.class;
		}
		if (c == OtpErlangString.class) {
			return String.class;
		}
		if (c == char.class) {
			return OtpErlangChar.class;
		}
		if (c == OtpErlangChar.class) {
			return char.class;
		}
		if (c.isArray()) {
			return OtpErlangTuple.class;
		}
		// if (c == OtpErlangTuple.class)
		// return .class;
		if (c == Object.class) {
			return OtpErlangList.class;
		}
		if (c == OtpErlangList.class) {
			return Object.class;
		}
		return null;
	}
}
