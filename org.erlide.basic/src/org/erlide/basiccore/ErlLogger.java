/*******************************************************************************
 * Copyright (c) 2007 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.basiccore;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

public class ErlLogger {

	private enum LogKind {
		DEBUG, INFO, WARN, ERROR
	};

	private static StackTraceElement getCaller() {
		StackTraceElement[] st = null;
		try {
			throw new Exception("");
		} catch (Exception e) {
			st = e.getStackTrace();
		}
		StackTraceElement el = null;
		int i = 0;
		do {
			el = st[i++];
		} while (el.getClassName().equals(ErlLogger.class.getName()));
		return el;
	}

	private static void log(LogKind kind, String fmt, Object... o) {
		StackTraceElement el = getCaller();
		String str = String.format(fmt, o);
		Date time = Calendar.getInstance().getTime();
		String stime = new SimpleDateFormat("HH:mm:ss,S").format(time);
		System.out.println("[" + kind.toString() + "] [" + stime + "] ("
				+ el.getFileName() + ":" + el.getLineNumber() + ") : " + str);
	}

	public static void debug(String fmt, Object... o) {
		log(LogKind.DEBUG, fmt, o);
	}

	public static void info(String fmt, Object... o) {
		log(LogKind.INFO, fmt, o);
	}

	public static void warn(String fmt, Object... o) {
		log(LogKind.WARN, fmt, o);
	}

	public static void error(String fmt, Object... o) {
		log(LogKind.ERROR, fmt, o);
	}
}
