package org.erlide.core.erlang.internal;

import org.eclipse.core.runtime.IPath;
import org.erlide.core.erlang.CPEntry;

public class CPEntryImpl implements CPEntry {

	private Iterable<IPath> excludes;
	private IPath outputLocation;
	private IPath path;
	private CPEntryKind kind;
	private Iterable<IPath> includes;
	private IPath includeLocation;
	private Iterable<CPEntry> libraryContents;

	public CPEntryImpl(CPEntryKind kind, IPath path, IPath outputLocation,
			Iterable<IPath> includes, Iterable<IPath> excludes,
			IPath includeLocation, Iterable<CPEntry> libraryContents) {
		this.kind = kind;
		this.path = path;
		this.outputLocation = outputLocation;
		this.includes = includes;
		this.excludes = excludes;
		this.includeLocation = includeLocation;
		this.libraryContents = libraryContents;
	}

	public Iterable<IPath> getExclusionPatterns() {
		return excludes;
	}

	public IPath getIncludeLocation() {
		return includeLocation;
	}

	public Iterable<IPath> getInclusionPatterns() {
		return includes;
	}

	public CPEntryKind getKind() {
		return kind;
	}

	public IPath getOutputLocation() {
		return outputLocation;
	}

	public IPath getPath() {
		return path;
	}

	public Iterable<CPEntry> getLibraryContents() {
		return libraryContents;
	}

}
