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
package org.erlide.core.internal.model.erlang;

import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.backend.runtimeinfo.RuntimeInfo;
import org.erlide.core.internal.model.root.ErlProjectInfo;
import org.erlide.core.internal.model.root.PathEntry;
import org.erlide.core.model.root.IOldErlangProjectProperties;
import org.erlide.jinterface.ErlLogger;
import org.erlide.utils.PreferencesUtils;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public final class PropertiesUtils {
    public static ErlProjectInfo convertOld(
            final IOldErlangProjectProperties old) throws URISyntaxException {
        ErlProjectInfo result = new ErlProjectInfo();
        result = result.setRequiredRuntimeVersion(old.getRuntimeVersion());
        if (!result.getRequiredRuntimeVersion().isDefined()) {
            final RuntimeInfo runtimeInfo = old.getRuntimeInfo();
            if (runtimeInfo != null) {
                result = result.setRequiredRuntimeVersion(runtimeInfo
                        .getVersion());
            }
        }

        // result =
        // result.getLayout().addSources(mkSources(old.getSourceDirs()));
        // result =
        // result.addIncludes(PreferencesUtils.unpackList(PathSerializer
        // .packList(old.getIncludeDirs())));
        // result = result.setOutput(old.getOutputDir());

        // final IPathVariableManager pvman = ResourcesPlugin.getWorkspace()
        // .getPathVariableManager();

        // final String exmodf = old.getExternalModulesFile();
        // URI ff = pvman.resolveURI(new URI(exmodf));
        // final List<String> externalModules = PreferencesUtils.readFile(ff
        // .toString());
        // final List<PathEntry> sloc = makeSourceLocations(externalModules);

        // final String exincf = old.getExternalModulesFile();
        // ff = pvman.resolveURI(new URI(exincf));
        // List<String> exinc = PreferencesUtils.readFile(ff.toString());
        // final List<IPath> externalIncludes = null;//
        // PreferencesUtils.unpackList(exinc);

        // final PathEntry loc = new PathEntry(sloc, externalIncludes, null,
        // null);
        // ArrayList<PathEntry> locs = new ArrayList<PathEntry>();
        // locs.add(loc);
        // result = result.addDependencies(locs);
        return result;
    }

    private static List<PathEntry> makeSourceLocations(
            final List<String> externalModules) {
        // FIXME!!!

        final List<PathEntry> result = Lists.newArrayList();

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
            List<String> pval = grouped.get(new Path(path));
            if (pval == null) {
                pval = Lists.newArrayList();
            }
            pval.add(file);
            grouped.put(new Path(path), pval);
        }
        ErlLogger.debug(grouped.toString());

        // for (final Entry<IPath, List<String>> loc : grouped.entrySet()) {
        // final SourceEntry location = new SourceEntry(loc.getKey(),
        // loc.getValue(), null, null, null, null);
        // result.add(location);
        // }

        return result;
    }

    private static List<PathEntry> mkSources(final Collection<IPath> list) {
        final List<PathEntry> result = Lists.newArrayList();
        // for (final IPath src : list) {
        // result.add(new SourceEntry(src, null, null, null, null, null));
        // }
        return result;
    }

    @SuppressWarnings("unchecked")
    public static <U, T extends U> Collection<T> filter(
            final Collection<U> dependencies2, final Class<T> class1) {
        final List<T> result = new ArrayList<T>();
        for (final U oo : dependencies2) {
            if (oo.getClass().equals(class1)) {
                result.add((T) oo);
            }
        }
        return result;
    }

    private PropertiesUtils() {
    }
}
