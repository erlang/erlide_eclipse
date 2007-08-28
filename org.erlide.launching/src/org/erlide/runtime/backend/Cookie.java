/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.Random;

/**
 * 
 * 
 * @author Vlad Dumitrescu [vlad_dumitrescu at hotmail dot com]
 */
public class Cookie {

	private static String fCookie = null;

	private Cookie() {
	}

	/**
	 * Start of lower case set
	 */
	private static final int UPPER_START_INT = 65;

	/**
	 * End of lowercase set
	 */
	private static final int UPPER_END_INT = 90;

	/**
	 * Start of lower case set
	 */
	private static final int LOWER_START_INT = 97;

	/**
	 * End of lowercase set
	 */
	private static final int LOWER_END_INT = 122;

	/**
	 * The lower range for the cookie
	 */
	private static final int LOWER_RANGE = LOWER_END_INT - LOWER_START_INT;

	/**
	 * The upper range of the cookie
	 */
	private static final int UPPER_RANGE = UPPER_END_INT - UPPER_START_INT;

	/**
	 * 
	 * @return
	 */
	public static String retrieveCookie() {
		if (fCookie != null) {
			return fCookie;
		}

		String cookieFileName = "";
		final File f;
		String cookie = "nocookie";

		cookieFileName = System.getProperty("user.home") + File.separator +
				".erlang.cookie";
		// ErlLogger.debug("Cookie file: " + cookieFileName);
		f = new File(cookieFileName);

		try {
			final FileReader fr = new FileReader(f);
			try {
				final char[] ar = new char[100];
				final int len = fr.read(ar);
				cookie = new String(ar, 0, len);
				char c;
				do {
					c = cookie.charAt(cookie.length() - 1);
					if (c == '\r' || c == '\n') {
						cookie = cookie.substring(0, cookie.length() - 1);
					}
				} while (c == '\r' || c == '\n');
			} finally {
				fr.close();
			}
		} catch (final FileNotFoundException e) {
			cookie = generateCookie();
		} catch (final IOException e) {
			cookie = generateCookie();
		}

		return cookie;
	}

	/**
	 * Generate an erlang node cookie.
	 * 
	 * @return The erlang node cookie
	 */
	private static String generateCookie() {
		Random ran;
		try {
			ran = SecureRandom.getInstance("SHA1PRNG");
		} catch (final NoSuchAlgorithmException nsae) {
			ran = new Random();
		}

		// Get the range of ascii alphabet
		final char start = ((char) ((ran.nextDouble() * LOWER_RANGE) + LOWER_START_INT));
		final StringBuffer buffer = new StringBuffer();
		buffer.append(start);

		// generate a 25 char string
		for (int i = 0; i < 25; i++) {
			if (ran.nextInt(2) == 0) {
				buffer
						.append((char) ((ran.nextDouble() * LOWER_RANGE) + LOWER_START_INT));
			} else {
				buffer
						.append((char) ((ran.nextDouble() * UPPER_RANGE) + UPPER_START_INT));
			}
		}
		fCookie = buffer.toString();
		return fCookie;
	}

}
