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

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.erlide.basicui.ErlideBasicUIPlugin;

public class ErlLogger {

	public enum Level {
		DEBUG(IStatus.INFO), INFO(IStatus.INFO), WARN(IStatus.WARNING), ERROR(
				IStatus.ERROR);

		private int lvl;

		private Level(int lvl) {
			this.lvl = lvl;
		}

		public int asInt() {
			return lvl;
		}
	};

	private static Level minLevel = Level.DEBUG;

	{
		String lvl = System.getProperty("erlide.logger.level");
		minLevel = lvl == null ? Level.INFO : Level.valueOf(lvl.toUpperCase());
	}

	public static void setLevel(Level level) {
		minLevel = level;
	}

	public static Level levelFromName(String levelName) {
		if ("info".equalsIgnoreCase(levelName)) {
			return Level.INFO;
		} else if ("debug".equalsIgnoreCase(levelName)) {
			return Level.DEBUG;
		} else if ("warn".equalsIgnoreCase(levelName)) {
			return Level.WARN;
		} else if ("error".equalsIgnoreCase(levelName)) {
			return Level.ERROR;
		} else {
			return minLevel;
		}
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
		if (kind.compareTo(minLevel) < 0) {
			return;
		}
		final StackTraceElement el = getCaller();
		final String str = String.format(fmt, o);
		// final Date time = Calendar.getInstance().getTime();
		// final String stime = new
		// SimpleDateFormat("HH:mm:ss,SSS").format(time);
		ErlideBasicUIPlugin.log(new Status(kind.asInt(),
				ErlideBasicUIPlugin.PLUGIN_ID, "[" + kind.toString() + "] ("
						+ el.getFileName() + ":" + el.getLineNumber() + ") : "
						+ str));
	}

	private static void log(Level kind, Exception e) {
		if (kind.compareTo(minLevel) < 0) {
			return;
		}
		final StackTraceElement el = getCaller();
		final String str = e.getMessage();

		// final Date time = Calendar.getInstance().getTime();
		// final String stime = new
		// SimpleDateFormat("HH:mm:ss,SSS").format(time);
		ErlideBasicUIPlugin.log(new Status(kind.asInt(),
				ErlideBasicUIPlugin.PLUGIN_ID, "[" + kind.toString() + "] ("
						+ el.getFileName() + ":" + el.getLineNumber() + ") : "
						+ str, e));
	}

	public static void erlangLog(String module, int line, Level kind,
			String fmt, Object... o) {
		if (kind.compareTo(minLevel) < 0) {
			return;
		}
		final String str = String.format(fmt, o);
		// final Date time = Calendar.getInstance().getTime();
		// final String stime = new
		// SimpleDateFormat("HH:mm:ss,SSS").format(time);
		ErlideBasicUIPlugin.log(new Status(kind.asInt(),
				ErlideBasicUIPlugin.PLUGIN_ID, "[" + kind.toString() + "] ("
						+ module + ":" + line + ") : " + str));
	}

	public static void debug(String fmt, Object... o) {
		log(Level.DEBUG, fmt, o);
	}

	public static void info(String fmt, Object... o) {
		log(Level.INFO, fmt, o);
	}

	public static void warn(String fmt, Object... o) {
		log(Level.WARN, fmt, o);
	}

	public static void error(String fmt, Object... o) {
		log(Level.ERROR, fmt, o);
	}

	public static void debug(Exception e) {
		log(Level.DEBUG, e);
	}

	public static void info(Exception e) {
		log(Level.INFO, e);
	}

	public static void warn(Exception e) {
		log(Level.WARN, e);
	}

	public static void error(Exception e) {
		log(Level.ERROR, e);
	}

}
