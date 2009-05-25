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
	public class Application {
		public IPath includeLocation;
		public IPath binariesLocation;
	}

	public class SourceApplication extends Application {
		public IPath sourceLocation;
		public Library buildDependencies;
	}

	public class Library {
		public Collection<Application> apps;
	}

	public class SystemLibrary extends Library { // ?
		public RuntimeVersion version;
	}

	public class Project {
		public Collection<Library> libraries;
		public Library sourceCode;
	}

}

