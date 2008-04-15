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

import java.io.PrintWriter;
import java.io.StringWriter;
import java.text.MessageFormat;
import java.util.Date;
import java.util.logging.Formatter;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

public class ErlLogger {

	private static int minLevel = Level.FINEST.intValue();

	{
		// This is not run?!?
		String lvl = System.getProperty("erlide.logger.level");
		minLevel = (lvl == null ? Level.INFO : Level.parse(lvl.toUpperCase()))
				.intValue();
	}

	private static StackTraceElement getCaller() {
		StackTraceElement[] st = Thread.currentThread().getStackTrace();
		StackTraceElement el = null;
		int i = 2;
		do {
			el = st[i++];
		} while (el.getClassName().equals(ErlLogger.class.getName()));
		return el;
	}

	private static void log(Level kind, String fmt, Object... o) {
		if (kind.intValue() < minLevel) {
			return;
		}
		final StackTraceElement el = getCaller();
		final String str = String.format(fmt, o);
		Logger.getLogger("org.erlide").finer(
				"(" + el.getFileName() + ":" + el.getLineNumber() + ") : "
						+ str);
	}

	private static void log(Level kind, Exception e) {
		if (kind.intValue() < minLevel) {
			return;
		}
		final StackTraceElement el = getCaller();
		final String str = e.getMessage();

		Logger.getLogger("org.erlide").log(
				java.util.logging.Level.FINER,
				"(" + el.getFileName() + ":" + el.getLineNumber() + ") : "
						+ str, e);
	}

	public static void erlangLog(String module, int line, String skind,
			String fmt, Object... o) {
		Level kind = Level.parse(skind);
		if (kind.intValue() < minLevel) {
			return;
		}
		final String str = String.format(fmt, o);
		Logger.getLogger("org.erlide").finer(
				"(" + module + ":" + line + ") : " + str);
	}

	public static void debug(String fmt, Object... o) {
		log(Level.FINEST, fmt, o);
	}

	public static void info(String fmt, Object... o) {
		log(Level.INFO, fmt, o);
	}

	public static void warn(String fmt, Object... o) {
		log(Level.WARNING, fmt, o);
	}

	public static void error(String fmt, Object... o) {
		log(Level.SEVERE, fmt, o);
	}

	public static void debug(Exception e) {
		log(Level.FINEST, e);
	}

	public static void info(Exception e) {
		log(Level.INFO, e);
	}

	public static void warn(Exception e) {
		log(Level.WARNING, e);
	}

	public static void error(Exception e) {
		log(Level.SEVERE, e);
	}

	public static class ErlSimpleFormatter extends Formatter {

		Date dat = new Date();
		private final static String format = "{0,time,HH:mm:ss,SSS}";
		private MessageFormat formatter;

		private Object args[] = new Object[1];

		private String lineSeparator = System.getProperty("line.separator");

		@Override
		public synchronized String format(LogRecord record) {
			StringBuffer sb = new StringBuffer();
			// Minimize memory allocations here.
			dat.setTime(record.getMillis());
			args[0] = dat;
			StringBuffer text = new StringBuffer();
			if (formatter == null) {
				formatter = new MessageFormat(format);
			}
			formatter.format(args, text, null);
			sb.append(text);
			sb.append(" ");
			String message = formatMessage(record);
			sb.append(record.getLevel());
			sb.append(": ");
			sb.append(message);
			sb.append(lineSeparator);
			if (record.getThrown() != null) {
				try {
					StringWriter sw = new StringWriter();
					PrintWriter pw = new PrintWriter(sw);
					record.getThrown().printStackTrace(pw);
					pw.close();
					sb.append(sw.toString());
				} catch (Exception ex) {
				}
			}
			return sb.toString();
		}
	}

}
