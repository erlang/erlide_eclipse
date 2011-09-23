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
package org.erlide.core.internal.backend;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.erlide.core.backend.BackendUtils;
import org.erlide.core.backend.ICodeBundle;
import org.erlide.core.common.Tuple;
import org.erlide.jinterface.ErlLogger;
import org.osgi.framework.Bundle;

import com.google.common.collect.Lists;

public class CodeBundleImpl implements ICodeBundle {

    private final Bundle bundle;
    private final Collection<Tuple<String, CodeContext>> paths;
    private final Collection<Tuple<String, String>> inits;

    public CodeBundleImpl(final Bundle b,
            final Collection<Tuple<String, CodeContext>> paths,
            final Collection<Tuple<String, String>> inits) {
        bundle = b;
        this.paths = Lists.newArrayList(paths);
        this.inits = inits;
    }

    public Bundle getBundle() {
        return bundle;
    }

    public Collection<String> getEbinDirs() {
        final List<String> result = Lists.newArrayList();
        for (final Tuple<String, CodeContext> path : paths) {
            final Collection<String> myPath = BeamUtil
                    .getPaths(path.o1, bundle);
            if (myPath != null) {
                result.addAll(myPath);
            } else {
                ErlLogger.warn("Can't access path %s, "
                        + "erlide plugins may be incorrectly built", path.o1);
            }
        }
        return result;
    }

    public Collection<String> getPluginCode() {
        final List<String> result = new ArrayList<String>();
        for (final Tuple<String, CodeContext> dir : paths) {
            final String beamModuleName = BackendUtils
                    .getBeamModuleName(dir.o1);
            if (beamModuleName != null) {
                result.add(beamModuleName);
            }
        }
        return result;
    }

    public Collection<Tuple<String, CodeContext>> getPaths() {
        return Collections.unmodifiableCollection(paths);
    }

    public Collection<Tuple<String, String>> getInits() {
        return Collections.unmodifiableCollection(inits);
    }

}
