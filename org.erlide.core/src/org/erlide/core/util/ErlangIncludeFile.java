package org.erlide.core.util;

public class ErlangIncludeFile {

	boolean systemInclude;

	String filename;

	/**
	 * @param systemInclude
	 * @param filename
	 */
	public ErlangIncludeFile(boolean systemInclude, String filename) {
		super();
		this.systemInclude = systemInclude;
		this.filename = filename;
	}

	public String getFilename() {
		return filename;
	}

	public boolean isSystemInclude() {
		return systemInclude;
	}

}
