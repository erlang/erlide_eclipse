/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.preferences;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.backend.util.PreferencesUtils;
import org.erlide.jinterface.util.ErlLogger;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public final class PropertiesUtils {
	public static ErlangProjectProperties convertOld(
			final OldErlangProjectProperties old) {
		ErlangProjectProperties result = new ErlangProjectProperties();
		result.setRequiredRuntimeVersion(old.getRuntimeVersion());
		if (!result.getRequiredRuntimeVersion().isDefined()) {
			final RuntimeInfo runtimeInfo = old.getRuntimeInfo();
			if (runtimeInfo != null) {
				result.setRequiredRuntimeVersion(runtimeInfo.getVersion());
			}
		}

		result.addSources(mkSources(old.getSourceDirs()));
		result.addIncludes(PreferencesUtils.unpackList(PathSerializer
				.packList(old.getIncludeDirs())));
		result.setOutput(old.getOutputDir());

		final IPathVariableManager pvman = ResourcesPlugin.getWorkspace()
				.getPathVariableManager();

		final String exmodf = old.getExternalModulesFile();
		IPath ff = pvman.resolvePath(new Path(exmodf));
		final List<String> externalModules = PreferencesUtils.readFile(ff
				.toString());
		final List<SourceLocation> sloc = makeSourceLocations(externalModules);

		final String exincf = old.getExternalModulesFile();
		ff = pvman.resolvePath(new Path(exincf));
		// List<String> exinc = PreferencesUtils.readFile(ff.toString());
		final List<IPath> externalIncludes = null;// PreferencesUtils.unpackList(exinc);

		final LibraryLocation loc = new LibraryLocation(sloc, externalIncludes,
				null, null);
		ArrayList<DependencyLocation> locs = new ArrayList<DependencyLocation>();
		locs.add(loc);
		result.addDependencies(locs);
		return result;
	}

	private static List<SourceLocation> makeSourceLocations(
			final List<String> externalModules) {
		final List<SourceLocation> result = Lists.newArrayList();

		final List<String> modules = Lists.newArrayList();
		for (final String mod : externalModules) {
			if (mod.endsWith(".erlidex")) {
				final List<String> mods = PreferencesUtils.readFile(mod);
				modules.addAll(mods);
			} else {
				modules.add(mod);
			}
		}

		final Map<IPath, List<String>> grouped = Maps.newHashMap();
		for (final String mod : modules) {
			final int i = mod.lastIndexOf('/');
			final String path = mod.substring(0, i);
			final String file = mod.substring(i + 1);

			ErlLogger.debug("FOUND: '" + path + "' '" + file + "'");
			List<String> pval = grouped.get(path);
			if (pval == null) {
				pval = Lists.newArrayList();
			}
			pval.add(file);
			grouped.put(new Path(path), pval);
		}
		ErlLogger.debug(grouped.toString());

		for (final Entry<IPath, List<String>> loc : grouped.entrySet()) {
			final SourceLocation location = new SourceLocation(loc.getKey(),
					loc.getValue(), null, null, null, null);
			result.add(location);
		}

		return result;
	}

	private static List<SourceLocation> mkSources(final Collection<IPath> list) {
		final List<SourceLocation> result = Lists.newArrayList();
		for (final IPath src : list) {
			result.add(new SourceLocation(src, null, null, null, null, null));
		}
		return result;
	}

	private PropertiesUtils() {
	}
}
