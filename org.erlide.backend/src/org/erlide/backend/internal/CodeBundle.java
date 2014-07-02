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
package org.erlide.backend.internal;

import java.net.URL;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.eclipse.xtext.xbase.lib.Pair;
import org.erlide.backend.api.ICodeBundle;
import org.erlide.backend.debug.BeamUtil;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.ErlLogger;
import org.osgi.framework.Bundle;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;
import com.google.common.collect.Sets;

public class CodeBundle implements ICodeBundle {

    private final RuntimeVersion version;
    private final Multimap<CodeContext, String> paths;
    private final Collection<Pair<String, String>> inits;
    private final Bundle bundle;

    public CodeBundle(final Bundle bundle, final RuntimeVersion version,
            final Multimap<CodeContext, String> paths,
            final Collection<Pair<String, String>> inits) {
        this.bundle = bundle;
        this.version = version;
        this.paths = HashMultimap.create();
        this.paths.putAll(paths);
        this.inits = inits;
    }

    @Override
    public Bundle getBundle() {
        return bundle;
    }

    @Override
    public Collection<String> getEbinDirs(final CodeContext context) {
        final Set<String> result = Sets.newHashSet();
        result.addAll(doGetEbinDirs(context));
        if (context != CodeContext.COMMON) {
            result.addAll(doGetEbinDirs(CodeContext.COMMON));
        }
        return result;
    }

    private List<String> doGetEbinDirs(final CodeContext context) {
        final List<String> result = Lists.newArrayList();
        for (final String path : paths.get(context)) {
            final String entryName = path.replace(" ", "%20");
            final URL entry = bundle.getEntry(entryName);
            if (entry != null) {
                final String aPath = BeamUtil.getPathFromUrl(entry);
                if (aPath != null) {
                    result.add(aPath);
                }
            } else {
                ErlLogger.warn("Can't access path %s, "
                        + "plugin %s may be incorrectly built (%s)", path,
                        bundle.getSymbolicName(), context.toString());
            }
        }
        return result;
    }

    @Override
    public Collection<Pair<String, String>> getInits() {
        return Collections.unmodifiableCollection(inits);
    }

    @Override
    public RuntimeVersion getVersion() {
        return version;
    }
}
