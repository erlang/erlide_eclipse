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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.erlide.runtime.PreferencesUtils;
import org.osgi.service.prefs.BackingStoreException;

public final class SourceLocation extends CodePathLocation {
	private String directory;
	private List<String> includePatterns = new ArrayList<String>();
	private List<String> excludePatterns = new ArrayList<String>();
	private String output;
	private Map<String, String> compilerOptions = new HashMap<String, String>();
	private Map<String, Map<String, String>> fileCompilerOptions = new HashMap<String, Map<String, String>>();

	public SourceLocation(final String directory,
			final List<String> includePatterns,
			final List<String> excludePatterns, final String output,
			final Map<String, String> compilerOptions,
			final Map<String, Map<String, String>> fileCompilerOptions) {
		super();
		Assert.isLegal(directory != null,
				"SourceLocation requires a non-null directory");
		this.directory = directory;
		if (includePatterns != null) {
			this.includePatterns = includePatterns;
		}
		if (excludePatterns != null) {
			this.excludePatterns = excludePatterns;
		}
		this.output = output;
		if (compilerOptions != null) {
			this.compilerOptions = compilerOptions;
		}
		if (fileCompilerOptions != null) {
			this.fileCompilerOptions = fileCompilerOptions;
		}
	}

	public SourceLocation(final IEclipsePreferences sn) {
		super();
		load(sn);
	}

	public String getDirectory() {
		return directory;
	}

	public Collection<String> getIncludePatterns() {
		return Collections.unmodifiableCollection(includePatterns);
	}

	public Collection<String> getExcludePatterns() {
		return Collections.unmodifiableCollection(excludePatterns);
	}

	public String getOutput() {
		return output;
	}

	public Map<String, String> getCompilerOptions() {
		return Collections.unmodifiableMap(compilerOptions);
	}

	public Map<String, Map<String, String>> getFileCompilerOptions() {
		return Collections.unmodifiableMap(fileCompilerOptions);
	}

	@Override
	public void load(final IEclipsePreferences root) {
		directory = root.get(ProjectPreferencesConstants.DIRECTORY, null);
		Assert.isLegal(directory != null,
				"SourceLocation requires a non-null directory");
	}

	@Override
	public void store(final IEclipsePreferences root)
			throws BackingStoreException {
		PreferencesUtils.clearAll(root);
		root.put(ProjectPreferencesConstants.DIRECTORY, directory);

	}

}
