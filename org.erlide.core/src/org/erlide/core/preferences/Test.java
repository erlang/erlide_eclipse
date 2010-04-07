package org.erlide.core.preferences;

import java.util.Collection;

import org.eclipse.core.runtime.IPath;
import org.erlide.jinterface.backend.RuntimeVersion;

/**
 * Temporary code, to see how things hang together.
 */
public class Test {

	public static class ProjectInfo {
		public Collection<SourceApplicationInfo> sources;
		public Collection<LibraryInfo> libraries;
		public IPath systemLibraryRef;
	}

	public static class LibraryInfo {
		public IPath root;
		public Collection<ApplicationInfo> apps;
	}

	public static class SystemLibraryInfo extends LibraryInfo {
		public RuntimeVersion version;
	}

	public static class ApplicationInfo {
		public IPath root;
		public IPath sourceLocation;
		public IPath includeLocation;
		public IPath binariesLocation;
	}

	public static class SourceApplicationInfo extends ApplicationInfo {
		public Collection<IPath> buildDependencyRefs;
	}

}
