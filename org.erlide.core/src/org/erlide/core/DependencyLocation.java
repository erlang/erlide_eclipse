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
import java.util.EnumSet;

public abstract class DependencyLocation extends CodePathLocation {
	public enum Kind {
		COMPILE_TIME, RUN_TIME
	};

	private EnumSet<Kind> kind;

	public abstract Collection<SourceLocation> getSources();

	public abstract Collection<String> getIncludes();

	public abstract String getOutput();

	public abstract Collection<LibraryLocation> getLibraries();

	public DependencyLocation(EnumSet<Kind> kind) {
		if (kind == null) {
			kind = EnumSet.of(Kind.RUN_TIME);
		}
		this.kind = kind;
	}

	public boolean isRunTime() {
		return kind.contains(Kind.RUN_TIME);
	}

	public boolean isCompileTime() {
		return kind.contains(Kind.COMPILE_TIME);
	}

}
