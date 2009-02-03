/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.erlang;

import org.eclipse.core.runtime.IPath;

public interface CPEntry {
	public enum CPEntryKind {
		// a source code location; not recursive; files can be excluded/included
		// by pattern; can have a separate output directory
		SOURCE,
		// a folder with include files
		INCLUDE,
		// another project in the workspace
		PROJECT,
		// an external library (beam + hrl, optional erl)
		LIBRARY,
		// a set of the above; is resolved in relation to the project by
		// ErlangCore
		CONTAINER,
	}

	// /////// all
	CPEntryKind getKind();

	IPath getPath();

	// /////// SOURCE
	IPath getOutputLocation();

	Iterable<IPath> getInclusionPatterns();

	Iterable<IPath> getExclusionPatterns();

	// /////// PROJECT
	IPath getIncludeLocation();

	// ///////// LIBRARY
	Iterable<CPEntry> getLibraryContents();

}
