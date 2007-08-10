/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend;

import java.util.List;

import org.eclipse.core.runtime.Plugin;

public interface ICodeManager {

	List<String> getPathA();

	List<String> getPathZ();

	void addPlugin(Plugin p);

	void removePlugin(Plugin p);

	void addPath(boolean usePathZ, String path);

	void removePath(boolean usePathZ, String path);

}
