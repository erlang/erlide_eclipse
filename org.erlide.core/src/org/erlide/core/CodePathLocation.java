/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.osgi.service.prefs.BackingStoreException;

public abstract class CodePathLocation {
	private static int gid = 1;

	public static synchronized int newId() {
		return gid++;
	}

	private int id;

	public CodePathLocation() {
		id = newId();
	}

	public int getId() {
		return id;
	}

	public abstract void load(IEclipsePreferences root)
			throws BackingStoreException;

	public abstract void store(IEclipsePreferences root)
			throws BackingStoreException;
}
