/*******************************************************************************
 * Copyright (c) 2005, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
/**
 * 
 */
package org.erlide.gunit.internal.ui;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;

public class TextualTrace {
	public static final int LINE_TYPE_EXCEPTION = 1;

	public static final int LINE_TYPE_NORMAL = 0;

	public static final int LINE_TYPE_STACKFRAME = 2;

	private final String fTrace;

	public TextualTrace(final String trace, final String[] filterPatterns) {
		super();
		this.fTrace = filterStack(trace, filterPatterns);
	}

	public void display(final ITraceDisplay display, final int maxLabelLength) {
		final StringReader stringReader = new StringReader(this.fTrace);
		final BufferedReader bufferedReader = new BufferedReader(stringReader);
		String line;

		try {
			// first line contains the thrown exception
			line = readLine(bufferedReader);
			if (line == null) {
				return;
			}

			displayWrappedLine(display, maxLabelLength, line,
					LINE_TYPE_EXCEPTION);

			// the stack frames of the trace
			while ((line = readLine(bufferedReader)) != null) {
				final int type = isAStackFrame(line) ? LINE_TYPE_STACKFRAME
						: LINE_TYPE_NORMAL;
				displayWrappedLine(display, maxLabelLength, line, type);
			}
		} catch (final IOException e) {
			display.addTraceLine(LINE_TYPE_NORMAL, this.fTrace);
		}
	}

	private void displayWrappedLine(final ITraceDisplay display, final int maxLabelLength,
			final String line, final int type) {
		final int labelLength = line.length();
		if (labelLength < maxLabelLength) {
			display.addTraceLine(type, line);
		} else {
			// workaround for bug 74647: JUnit view truncates
			// failure message
			display.addTraceLine(type, line.substring(0, maxLabelLength));
			int offset = maxLabelLength;
			while (offset < labelLength) {
				final int nextOffset = Math.min(labelLength, offset + maxLabelLength);
				display.addTraceLine(LINE_TYPE_NORMAL, line.substring(offset,
						nextOffset));
				offset = nextOffset;
			}
		}
	}

	private boolean filterLine(final String[] patterns, final String line) {
		String pattern;
		int len;
		for (int i = (patterns.length - 1); i >= 0; --i) {
			pattern = patterns[i];
			len = pattern.length() - 1;
			if (pattern.charAt(len) == '*') {
				// strip trailing * from a package filter
				pattern = pattern.substring(0, len);
			} else if (Character.isUpperCase(pattern.charAt(0))) {
				// class in the default package
				pattern = FailureTrace.FRAME_PREFIX + pattern + '.';
			} else {
				// class names start w/ an uppercase letter after the .
				final int lastDotIndex = pattern.lastIndexOf('.');
				if ((lastDotIndex != -1)
						&& (lastDotIndex != len)
						&& Character.isUpperCase(pattern
								.charAt(lastDotIndex + 1))) {
					pattern += '.'; // append . to a class filter
				}
			}

			if (line.indexOf(pattern) > 0) {
				return true;
			}
		}
		return false;
	}

	private String filterStack(final String stackTrace, final String[] filterPatterns) {
		if (filterPatterns.length == 0 || stackTrace == null) {
			return stackTrace;
		}

		final StringWriter stringWriter = new StringWriter();
		final PrintWriter printWriter = new PrintWriter(stringWriter);
		final StringReader stringReader = new StringReader(stackTrace);
		final BufferedReader bufferedReader = new BufferedReader(stringReader);

		String line;
		final String[] patterns = filterPatterns;
		try {
			while ((line = bufferedReader.readLine()) != null) {
				if (!filterLine(patterns, line)) {
					printWriter.println(line);
				}
			}
		} catch (final IOException e) {
			return stackTrace; // return the stack unfiltered
		}
		return stringWriter.toString();
	}

	private boolean isAStackFrame(final String itemLabel) {
		// heuristic for detecting a stack frame - works for JDK
		return itemLabel.indexOf(" at ") >= 0; //$NON-NLS-1$
	}

	private String readLine(final BufferedReader bufferedReader) throws IOException {
		final String readLine = bufferedReader.readLine();
		return readLine == null ? null : readLine.replace('\t', ' ');
	}
}
