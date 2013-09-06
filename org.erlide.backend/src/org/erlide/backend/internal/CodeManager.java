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
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IContributor;
import org.erlide.backend.BackendUtils;
import org.erlide.backend.api.ICodeBundle;
import org.erlide.runtime.api.BeamLoader;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.RuntimeUtils;
import org.erlide.util.ErlLogger;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangBinary;

public class CodeManager {

    private final IRpcSite site;
    private final String backendName;
    private final List<PathItem> pathA;
    private final List<PathItem> pathZ;
    private final List<ICodeBundle> registeredBundles;

    // only to be called by Backend
    CodeManager(final IRpcSite site, final String backendName) {
        this.site = site;
        this.backendName = backendName;
        pathA = new ArrayList<PathItem>();
        pathZ = new ArrayList<PathItem>();
        registeredBundles = new ArrayList<ICodeBundle>();
    }

    public void addPath(final boolean usePathZ, final String path) {
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

    public void removePath(final String path) {
        if (removePath(pathA, path)) {
            ErlangCode.removePath(site, path);
        }
    }

    public void register(final ICodeBundle b) {
        registeredBundles.add(b);
        registerBundle(b);
    }

    public void unregister(final ICodeBundle bundle) {
        if (bundle == null) {
            return;
        }
        registeredBundles.remove(bundle);
        unloadPluginCode(bundle);
    }

    /**
     * @param moduleName
     * @param beamPath
     * @return boolean
     */
    private boolean loadBeam(final String moduleName, final URL beamPath) {
        final OtpErlangBinary bin = BeamUtil
                .getBeamBinary(moduleName, beamPath);
        if (bin == null) {
            return false;
        }
        return BeamLoader.loadBeam(site, moduleName, bin);
    }

    private void loadPluginCode(final ICodeBundle p) {

        final Bundle b = p.getBundle();
        ErlLogger.debug("loading plugin " + b.getSymbolicName() + " in "
                + backendName);

        // TODO Do we have to also check any fragments?
        // see FindSupport.findInFragments

        final IConfigurationElement[] els = BackendUtils
                .getCodepathConfigurationElements();
        for (final IConfigurationElement el : els) {
            final IContributor c = el.getContributor();
            if ("beam_dir".equals(el.getName())
                    && c.getName().equals(b.getSymbolicName())) {
                final String dirPath = el.getAttribute("path");
                @SuppressWarnings("rawtypes")
                Enumeration e = null;
                if (dirPath != null) {
                    e = b.getEntryPaths(dirPath);
                }
                if (e == null) {
                    ErlLogger.warn("Could not find Erlang code in plugin "
                            + b.getSymbolicName());
                    return;
                }
                while (e.hasMoreElements()) {
                    final String s = (String) e.nextElement();
                    final String beamModuleName = BackendUtils
                            .getBeamModuleName(s);
                    if (beamModuleName != null) {
                        // ErlLogger.debug(" " + beamModuleName);
                        try {
                            final boolean ok = loadBeam(beamModuleName,
                                    b.getEntry(s));
                            if (!ok) {
                                ErlLogger.error("Could not load %s",
                                        beamModuleName);
                            }
                        } catch (final Exception ex) {
                            ErlLogger.warn(ex);
                        }
                    }
                }
            }
        }

        // loadStubCode(b, reg);
        // ErlLogger.debug("*done! loading plugin " + b.getSymbolicName());
    }

    private boolean addPath(final List<PathItem> l, final String path) {
        if (path == null) {
            return false;
        }

        final PathItem it = findItem(l, path);
        if (it == null) {
            l.add(new PathItem(path));
            return true;
        }
        it.incRef();
        return false;
    }

    private boolean removePath(final List<PathItem> l, final String path) {
        if (path == null) {
            return false;
        }
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

    private void registerBundle(final ICodeBundle p) {
        final String externalPath = System.getProperty(p.getBundle()
                .getSymbolicName() + ".ebin");
        if (externalPath != null) {
            final boolean accessible = RuntimeUtils.isAccessibleDir(site,
                    externalPath);
            if (accessible) {
                ErlLogger.debug("adding external %s to code path for %s:: %s",
                        externalPath, site, backendName);
                ErlangCode.addPathA(site, externalPath);
                return;
            }
            ErlLogger.info("external code path %s for %s "
                    + "is not accessible, using plugin code", externalPath,
                    site, backendName);
        }
        final Collection<String> ebinDirs = p.getEbinDirs();
        if (ebinDirs != null) {
            for (final String ebinDir : ebinDirs) {
                final String localDir = ebinDir.replaceAll("\\\\", "/");
                final boolean accessible = RuntimeUtils.isAccessibleDir(site,
                        localDir);
                final boolean embedded = ErlangCode.isEmbedded(site);
                if (accessible && !embedded) {
                    ErlLogger.debug("adding %s to code path for @%s:: %s",
                            localDir, site.hashCode(), backendName);
                    ErlangCode.addPathA(site, localDir);
                } else {
                    ErlLogger.debug("loading %s for %s", p.getBundle(),
                            backendName);
                    loadPluginCode(p);
                }
            }
        } else {
            ErlLogger
                    .warn("Could not find 'ebin' in bundle %s.", p.getBundle());
            loadPluginCode(p);
        }
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

    private void unloadPluginCode(final ICodeBundle p) {
        final Bundle b = p.getBundle();
        @SuppressWarnings("rawtypes")
        Enumeration e;
        ErlLogger.debug("*> really unloading plugin " + p.getClass().getName());
        e = b.getEntryPaths("/ebin");
        while (e.hasMoreElements()) {
            final String s = (String) e.nextElement();
            final String beamModuleName = BackendUtils.getBeamModuleName(s);
            if (beamModuleName != null) {
                unloadBeam(beamModuleName);
            }
        }
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
