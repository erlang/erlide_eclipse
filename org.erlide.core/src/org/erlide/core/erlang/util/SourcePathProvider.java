package org.erlide.core.erlang.util;

import java.util.Collection;

import org.eclipse.core.runtime.IPath;

public interface SourcePathProvider {

	Collection<IPath> getSourcePaths();
	
}
