/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.builder;

import java.text.MessageFormat;

import org.eclipse.osgi.util.NLS;

public final class BuilderMessages extends NLS {

	private static final String BUNDLE_NAME = "org.erlide.core.builder.messages"; //$NON-NLS-1$

	private BuilderMessages() {
	}

	public static String build_oneWarning;
	public static String build_multipleWarnings;
	public static String build_multipleErrors;
	public static String build_oneError;
	public static String build_compiling;
	public static String build_done;
	public static String build_foundHeader;
	public static String build_fixedHeader;
	public static String build_inconsistentProject;
	public static String build_missingSourceFile;
	public static String build_preparingBuild;
	public static String build_readingDelta;

	static {
		NLS.initializeMessages(BUNDLE_NAME, BuilderMessages.class);
	}

	/**
	 * Bind the given message's substitution locations with the given string
	 * values.
	 * 
	 * @param message
	 *            the message to be manipulated
	 * @return the manipulated String
	 */
	public static String bind(final String message) {
		return bind(message, null);
	}

	/**
	 * Bind the given message's substitution locations with the given string
	 * values.
	 * 
	 * @param message
	 *            the message to be manipulated
	 * @param binding
	 *            the object to be inserted into the message
	 * @return the manipulated String
	 */
	public static String bind(final String message, final Object binding) {
		return bind(message, new Object[] { binding });
	}

	/**
	 * Bind the given message's substitution locations with the given string
	 * values.
	 * 
	 * @param message
	 *            the message to be manipulated
	 * @param binding1
	 *            An object to be inserted into the message
	 * @param binding2
	 *            A second object to be inserted into the message
	 * @return the manipulated String
	 */
	public static String bind(final String message, final Object binding1,
			final Object binding2) {
		return bind(message, new Object[] { binding1, binding2 });
	}

	/**
	 * Bind the given message's substitution locations with the given string
	 * values.
	 * 
	 * @param message
	 *            the message to be manipulated
	 * @param bindings
	 *            An array of objects to be inserted into the message
	 * @return the manipulated String
	 */
	public static String bind(final String message, final Object[] bindings) {
		return MessageFormat.format(message, bindings);
	}
}
