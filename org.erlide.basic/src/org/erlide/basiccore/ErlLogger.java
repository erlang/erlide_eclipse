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

public class ErlLogger {

	public static void log(Object o) {
		StackTraceElement[] st = null;
		try {
			throw new Exception("");
		} catch (Exception e) {
			st = e.getStackTrace();
		}
		StackTraceElement el = st[1];
		System.out.println("ERL (" + el.getFileName() + ":"
				+ el.getLineNumber() + ") : " + o.toString());
	}
}
