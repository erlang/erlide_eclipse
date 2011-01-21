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
package org.erlide.runtime.backend.internal;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IContributor;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.RegistryFactory;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.ErlBackend;
import org.erlide.jinterface.backend.ErlangCode;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.CodeBundle;
import org.erlide.runtime.backend.ErlideBackend;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangBinary;

public class CodeManager {

    private final Backend backend;

    private final List<PathItem> pathA;
    private final List<PathItem> pathZ;

    private final List<CodeBundle> registeredBundles;

    // only to be called by ErlideBackend
    public CodeManager(final ErlideBackend b) {
        backend = b;
        pathA = new ArrayList<PathItem>();
        pathZ = new ArrayList<PathItem>();
        registeredBundles = new ArrayList<CodeBundle>();
    }

    public void addPath(final boolean usePathZ, final String path) {
        if (usePathZ) {
            if (addPath(pathZ, path)) {
                ErlangCode.addPathZ(backend, path);
            }
        } else {
            if (addPath(pathA, path)) {
                ErlangCode.addPathA(backend, path);
            }
        }
    }

    public void removePath(final String path) {
        if (removePath(pathA, path)) {
            ErlangCode.removePath(backend, path);
        }
    }

    public void reRegisterBundles() {
        for (final CodeBundle p : registeredBundles) {
            registerBundle(p);
        }
    }

    public void register(final CodeBundle b) {
        registeredBundles.add(b);
        registerBundle(b);
    }

    public void unregister(final Bundle b) {
        final CodeBundle p = findBundle(b);
        if (p == null) {
            return;
        }
        registeredBundles.remove(p);
        unloadPluginCode(p);
    }

    /**
     * @param moduleName
     * @param beamPath
     * @return boolean
     */
    private boolean loadBeam(final String moduleName, final URL beamPath) {
        final OtpErlangBinary bin = ErlideUtil.getBeamBinary(moduleName,
                beamPath);
        if (bin == null) {
            return false;
        }
        return ErlBackend.loadBeam(backend, moduleName, bin);
    }

    private void loadPluginCode(final CodeBundle p) {

        final Bundle b = p.getBundle();
        ErlLogger.debug("loading plugin " + b.getSymbolicName() + " in "
                + backend.getInfo().getName());

        // TODO Do we have to also check any fragments?
        // see FindSupport.findInFragments

        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        // reg.addRegistryChangeListener(this);
        final IConfigurationElement[] els = reg.getConfigurationElementsFor(
                ErlangPlugin.PLUGIN_ID, "codepath");
        for (final IConfigurationElement el : els) {
            final IContributor c = el.getContributor();
            if ("beam_dir".equals(el.getName())
                    && c.getName().equals(b.getSymbolicName())) {
                final String dir_path = el.getAttribute("path");
                final String ver = backend.getCurrentVersion();
                @SuppressWarnings("rawtypes")
                Enumeration e = null;
                if (dir_path != null) {
                    e = b.getEntryPaths(dir_path + "/" + ver);
                    if (e == null || !e.hasMoreElements()) {
                        e = b.getEntryPaths(dir_path);
                    }
                }
                if (e == null) {
                    ErlLogger.debug("* !!! error loading plugin "
                            + b.getSymbolicName());
                    return;
                }
                while (e.hasMoreElements()) {
                    final String s = (String) e.nextElement();
                    final Path path = new Path(s);
                    if (path.getFileExtension() != null
                            && "beam".compareTo(path.getFileExtension()) == 0) {
                        final String m = path.removeFileExtension()
                                .lastSegment();
                        // ErlLogger.debug(" " + m);
                        try {
                            final boolean ok = loadBeam(m, b.getEntry(s));
                            if (!ok) {
                                ErlLogger.error("Could not load %s", m);
                            }
                            ErlangCore.getBackendManager().moduleLoaded(
                                    backend, null, m);
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

    private void registerBundle(final CodeBundle p) {
        final String externalPath = System.getProperty(p.getBundle()
                .getSymbolicName() + ".ebin");
        if (externalPath != null) {
            final boolean accessible = ErlideUtil.isAccessible(backend,
                    externalPath);
            if (accessible) {
                ErlLogger.debug("adding %s to code path for %s:: %s",
                        externalPath, backend, backend.getInfo());
                ErlangCode.addPathA(backend, externalPath);
                return;
            } else {
                ErlLogger.info("external code path %s for %s "
                        + "is not accessible, using plugin code", externalPath,
                        backend, backend.getInfo());
            }
        }
        final Collection<String> ebinDirs = p.getEbinDirs();
        if (ebinDirs != null) {
            for (final String ebinDir : ebinDirs) {
                final String localDir = ebinDir.replaceAll("\\\\", "/");
                final boolean accessible = ErlideUtil.isAccessible(backend,
                        localDir);
                if (accessible) {
                    ErlLogger.debug("adding %s to code path for %s:: %s",
                            localDir, backend, backend.getInfo());
                    ErlangCode.addPathA(backend, localDir);
                } else {
                    ErlLogger.debug("loading %s for %s", p.getBundle()
                            .getSymbolicName(), backend.getInfo());
                    loadPluginCode(p);
                }
            }
        } else {
            ErlLogger.warn("Could not find 'ebin' in bundle %s.", p.getBundle()
                    .getSymbolicName());
            loadPluginCode(p);
        }
    }

    private CodeBundle findBundle(final Bundle b) {
        for (final CodeBundle p : registeredBundles) {
            if (p.getBundle() == b) {
                return p;
            }
        }
        return null;
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

    private void unloadPluginCode(final CodeBundle p) {
        // TODO Do we have to also check any fragments?
        // see FindSupport.findInFragments

        final Bundle b = p.getBundle();
        final String ver = backend.getCurrentVersion();
        @SuppressWarnings("rawtypes")
        Enumeration e = b.getEntryPaths("/ebin/" + ver);
        if (e == null) {
            return;
        }
        ErlLogger.debug("*> really unloading plugin " + p.getClass().getName());
        if (!e.hasMoreElements()) {
            e = b.getEntryPaths("/ebin");
        }
        while (e.hasMoreElements()) {
            final String s = (String) e.nextElement();
            final Path path = new Path(s);
            if (path.getFileExtension() != null
                    && "beam".compareTo(path.getFileExtension()) == 0) {
                final String m = path.removeFileExtension().lastSegment();
                unloadBeam(m);
            }
        }
    }

    private void unloadBeam(final String moduleName) {
        ErlangCode.delete(backend, moduleName);
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
