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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.backend.BackendUtils;
import org.erlide.backend.api.ICodeBundle;
import org.erlide.backend.api.ICodeBundle.CodeContext;
import org.erlide.backend.debug.BeamUtil;
import org.erlide.runtime.api.BeamLoader;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangBinary;

public class CodeManager {

    private final IOtpRpc site;
    private final String backendName;
    private final List<PathItem> pathA;
    private final List<PathItem> pathZ;
    private final RuntimeVersion version;

    // only to be called by Backend
    CodeManager(final IOtpRpc site, final String backendName,
            final RuntimeVersion version) {
        this.site = site;
        this.backendName = backendName;
        this.version = version;
        pathA = new ArrayList<PathItem>();
        pathZ = new ArrayList<PathItem>();
    }

    public void addPath(final boolean usePathZ, final @NonNull String path) {
        if (usePathZ) {
            if (addPath(pathZ, path)) {
                ErlangCode.addPathZ(site, path);
            }
        } else {
            if (addPath(pathA, path)) {
                ErlangCode.addPathA(site, path);
            }
        }
    }

    public void removePath(final @NonNull String path) {
        if (removePath(pathA, path)) {
            ErlangCode.removePath(site, path);
        }
    }

    public void register(final CodeContext context, final ICodeBundle bundle) {
        if (bundle.getVersion() != RuntimeVersion.NO_VERSION
                && bundle.getVersion().getMajor() != version.getMajor()) {
            return;
        }
        final Collection<String> ebinDirs = bundle.getEbinDirs(context);
        if (ebinDirs == null) {
            ErlLogger.warn("Could not find 'ebin' in bundle %s.", bundle.getBundle());
            return;
        }
        for (final String ebinDir : ebinDirs) {
            final String localDir = ebinDir.replaceAll("\\\\", "/");
            final boolean accessible = BackendUtils.isAccessibleDir(site, localDir);
            final boolean embedded = ErlangCode.isEmbedded(site);
            if (accessible && !embedded) {
                ErlangCode.addPathA(site, localDir);
            } else {
                ErlLogger.debug("loading %s for %s", bundle.getBundle(), backendName);
                loadCodeForBundle(context, bundle);
            }
        }
    }

    public void unregister(final CodeContext context, final ICodeBundle bundle) {
        if (bundle == null) {
            return;
        }
        ErlLogger.debug("unloading %s for %s", bundle.getBundle(), backendName);
        unloadCodeForBundle(context, bundle);
    }

    private boolean loadBeam(final String moduleName, final URL beamPath) {
        final OtpErlangBinary bin = BeamUtil.getBeamBinary(moduleName, beamPath);
        if (bin == null) {
            return false;
        }
        return BeamLoader.loadBeam(site, moduleName, bin);
    }

    private void loadCodeForBundle(final CodeContext context, final ICodeBundle bundle) {
        final Collection<String> ebinDirs = bundle.getEbinDirs(context);
        if (ebinDirs == null) {
            return;
        }
        for (final String ebinDir : ebinDirs) {
            final String beamModuleName = BackendUtils.getBeamModuleName(ebinDir);
            if (beamModuleName != null) {
                // ErlLogger.debug(" load " + beamModuleName);
                final URL entry = bundle.getBundle().getEntry(ebinDir);
                final boolean ok = entry != null && loadBeam(beamModuleName, entry);
                if (!ok) {
                    ErlLogger.error("Could not load %s", beamModuleName);
                }
            }
        }
    }

    private void unloadCodeForBundle(final CodeContext context, final ICodeBundle bundle) {
        final Collection<String> ebinDirs = bundle.getEbinDirs(context);
        if (ebinDirs == null) {
            return;
        }
        for (final String ebinDir : ebinDirs) {
            final String beamModuleName = BackendUtils.getBeamModuleName(ebinDir);
            if (beamModuleName != null) {
                // ErlLogger.debug(" unload " + beamModuleName);
                unloadBeam(beamModuleName);
            }
        }
    }

    private boolean addPath(final List<PathItem> l, final @NonNull String path) {
        final PathItem it = findItem(l, path);
        if (it == null) {
            l.add(new PathItem(path));
            return true;
        }
        it.incRef();
        return false;
    }

    private boolean removePath(final List<PathItem> l, final @NonNull String path) {
        final PathItem it = findItem(l, path);
        if (it != null) {
            it.decRef();
            if (it.ref <= 0) {
                l.remove(it);
                return true;
            }
        }
        return false;
    }

    private PathItem findItem(final List<PathItem> l, final String p) {
        final Iterator<PathItem> i = l.iterator();
        while (i.hasNext()) {
            final PathItem it = i.next();
            if (it.path.equals(p)) {
                return it;
            }
        }
        return null;
    }

    private void unloadBeam(final String moduleName) {
        ErlangCode.delete(site, moduleName);
    }

    private static class PathItem {

        public PathItem(final String p) {
            path = p;
            ref = 1;
        }

        public String path;

        public int ref;

        public void incRef() {
            ref++;
        }

        public void decRef() {
            ref--;
        }
    }

}
