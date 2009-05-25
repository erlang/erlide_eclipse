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
package org.erlide.core.preferences;

import java.util.Collection;
import java.util.EnumSet;

public abstract class DependencyLocation extends CodePathLocation {
	public enum Kind {
		COMPILE_TIME, RUN_TIME
	};

	private final EnumSet<Kind> kind;

	public abstract Collection<SourceLocation> getSources();

	public abstract Collection<String> getIncludes();

	public abstract String getOutput();

	public abstract Collection<DependencyLocation> getDependencies();

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
