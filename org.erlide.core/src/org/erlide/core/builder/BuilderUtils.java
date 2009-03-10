/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.builder;

import org.eclipse.core.runtime.Platform;
import org.erlide.core.ErlangPlugin;

public class BuilderUtils {
	private BuilderUtils() {
	}

	public static boolean isDebugging() {
		if (ErlangPlugin.getDefault() == null) {
			return false;
		}
		return ErlangPlugin.getDefault().isDebugging()
				&& Platform.getDebugOption("org.erlide.core/debug/builder")
						.equals("true");
	}
}
