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

	void addPathA(String path);

	void addPathZ(String path);

	void removePathA(String path);

	void removePathZ(String path);

	List getPathA();

	List getPathZ();

	void addPlugin(Plugin p);

	void removePlugin(Plugin p);

}
