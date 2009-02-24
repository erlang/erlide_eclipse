/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core;

import java.util.Collection;

public abstract class DependencyLocation extends CodePathLocation {
	public abstract Collection<SourceLocation> getSources();

	public abstract Collection<String> getIncludes();

	public abstract String getOutput();

	public abstract Collection<LibraryLocation> getLibraries();

}
