package org.erlide.core.preferences;

import java.util.Collection;

import org.eclipse.core.runtime.IPath;
import org.erlide.jinterface.backend.RuntimeVersion;

/**
 * Temporary code, to see how things hang together.
 * 
 * @author Vlad
 * 
 */
public class Test {
	public class ProjectInfo {
		public Collection<SourceApplicationInfo> sources;
		public Collection<LibraryInfo> libraries;
	}

	public class LibraryInfo {
		public Collection<ApplicationInfo> apps;
	}

	public class SystemLibraryInfo extends LibraryInfo {
		public RuntimeVersion version;
	}

	public class ApplicationInfo {
		public IPath includeLocation;
		public IPath binariesLocation;
	}

	public class SourceApplicationInfo extends ApplicationInfo {
		public IPath sourceLocation;

		// how do we refer to these? (externally)
		public Collection<ApplicationInfo> buildDependencies;
	}

}
