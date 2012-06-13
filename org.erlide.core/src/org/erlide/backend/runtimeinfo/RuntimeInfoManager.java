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
package org.erlide.backend.runtimeinfo;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.erlide.core.ErlangCore;
import org.erlide.jinterface.ErlLogger;
import org.erlide.utils.PreferencesUtils;
import org.osgi.service.prefs.BackingStoreException;

import com.ericsson.otp.erlang.RuntimeVersion;
import com.google.common.collect.Lists;

public final class RuntimeInfoManager implements IPreferenceChangeListener {

    private RuntimeInfo erlideRuntime;
    private final Map<String, RuntimeInfo> fRuntimes = new HashMap<String, RuntimeInfo>();
    private String defaultRuntimeName = "";
    private final List<RuntimeInfoListener> fListeners = new ArrayList<RuntimeInfoListener>();

    public RuntimeInfoManager() {
        getRootPreferenceNode().addPreferenceChangeListener(this);
        load();
        initializeRuntimesList();
        setDefaultRuntimes();
    }

    public synchronized Collection<RuntimeInfo> getRuntimes() {
        return new ArrayList<RuntimeInfo>(fRuntimes.values());
    }

    public synchronized void store() {
        IEclipsePreferences root = getRootPreferenceNode();
        try {
            root.removePreferenceChangeListener(this);
            root.removeNode();
            root = getRootPreferenceNode();

            for (final RuntimeInfo rt : fRuntimes.values()) {
                final RuntimeInfoLoader rtl = new RuntimeInfoLoader(rt);
                rtl.store(root);
            }
            if (defaultRuntimeName != null) {
                root.put("default", defaultRuntimeName);
            }
            if (erlideRuntime != null) {
                root.put("erlide", erlideRuntime.getName());
            }
            try {
                root.flush();
            } catch (final BackingStoreException e) {
                ErlLogger.warn(e);
            }
            root.addPreferenceChangeListener(this);
        } catch (final BackingStoreException e) {
            ErlLogger.warn(e);
        }
    }

    public synchronized void load() {
        fRuntimes.clear();
        loadDefaultPrefs();

        IEclipsePreferences root = new DefaultScope()
                .getNode(ErlangCore.PLUGIN_ID + "/runtimes");
        loadPrefs(root);
        root = getRootPreferenceNode();
        loadPrefs(root);
    }

    private void loadDefaultPrefs() {
        final IPreferencesService ps = Platform.getPreferencesService();
        final String DEFAULT_ID = "org.erlide";

        final String defName = ps.getString(DEFAULT_ID, "default_name", null,
                null);
        final RuntimeInfo runtime = getRuntime(defName);
        if (defName != null && runtime == null) {
            final RuntimeInfo rt = new RuntimeInfo();
            rt.setName(defName);
            final String path = ps.getString(DEFAULT_ID, "default_"
                    + RuntimeInfoLoader.CODE_PATH, "", null);
            rt.setCodePath(PreferencesUtils.unpackList(path));
            rt.setOtpHome(ps.getString(DEFAULT_ID, "default_"
                    + RuntimeInfoLoader.HOME_DIR, "", null));
            rt.setArgs(ps.getString(DEFAULT_ID, "default_"
                    + RuntimeInfoLoader.ARGS, "", null));
            addRuntime(rt);
        }
        defaultRuntimeName = defName;
    }

    private void loadPrefs(final IEclipsePreferences root) {
        final String defrt = root.get("default", null);
        if (defrt != null) {
            defaultRuntimeName = defrt;
        }

        String[] children;
        try {
            children = root.childrenNames();
            for (final String name : children) {
                final RuntimeInfo rt = new RuntimeInfo();
                final RuntimeInfoLoader rtl = new RuntimeInfoLoader(rt);
                rtl.load(root.node(name));
                fRuntimes.put(name, rt);
            }
        } catch (final BackingStoreException e) {
            ErlLogger.warn(e);
        }

        if (getDefaultRuntime() == null) {
            if (defaultRuntimeName == null && fRuntimes.size() > 0) {
                defaultRuntimeName = fRuntimes.values().iterator().next()
                        .getName();
            }
        }
    }

    protected IEclipsePreferences getRootPreferenceNode() {
        return new InstanceScope().getNode(ErlangCore.PLUGIN_ID + "/runtimes");
    }

    public synchronized void setRuntimes(final Collection<RuntimeInfo> elements) {
        fRuntimes.clear();
        for (final RuntimeInfo rt : elements) {
            fRuntimes.put(rt.getName(), rt);
        }
        notifyListeners();
    }

    public synchronized void addRuntime(final RuntimeInfo rt) {
        if (!fRuntimes.containsKey(rt.getName())) {
            fRuntimes.put(rt.getName(), rt);
        }
        notifyListeners();
    }

    public synchronized Collection<String> getRuntimeNames() {
        return fRuntimes.keySet();
    }

    public boolean hasRuntimeWithName(final String name) {
        for (final RuntimeInfo vm : fRuntimes.values()) {
            if (vm.getName().equals(name)) {
                return true;
            }
        }
        return false;
    }

    public RuntimeInfo getRuntime(final String name) {
        final RuntimeInfo rt = fRuntimes.get(name);
        return rt;
    }

    public synchronized void removeRuntime(final String name) {
        fRuntimes.remove(name);
        if (erlideRuntime.getName().equals(name)) {
            erlideRuntime = fRuntimes.values().iterator().next();
        }
        if (defaultRuntimeName.equals(name)) {
            defaultRuntimeName = fRuntimes.keySet().iterator().next();
        }
        notifyListeners();
    }

    public synchronized String getDefaultRuntimeName() {
        return defaultRuntimeName;
    }

    public synchronized void setDefaultRuntime(final String name) {
        defaultRuntimeName = name;
        notifyListeners();
    }

    private synchronized void setErlideRuntime(final RuntimeInfo runtime) {
        final RuntimeInfo old = erlideRuntime;
        if (old == null || !old.equals(runtime)) {
            erlideRuntime = runtime;
            notifyListeners();
            // this creates infinite recursion!
            // BackendManagerImpl.getDefault().getIdeBackend().stop();
        }
    }

    public synchronized RuntimeInfo getErlideRuntime() {
        return erlideRuntime;
    }

    public synchronized RuntimeInfo getDefaultRuntime() {
        return getRuntime(getDefaultRuntimeName());
    }

    @Override
    public void preferenceChange(final PreferenceChangeEvent event) {
        if (event.getNode().absolutePath().contains("org.erlide")) {
            load();
        }
    }

    public void addListener(final RuntimeInfoListener listener) {
        if (!fListeners.contains(listener)) {
            fListeners.add(listener);
        }
    }

    public void removeListener(final RuntimeInfoListener listener) {
        fListeners.remove(listener);
    }

    private void notifyListeners() {
        for (final RuntimeInfoListener listener : fListeners) {
            listener.infoChanged();
        }
    }

    /**
     * Locate runtimes with this version or newer. If exact matches exists, they
     * are first in the result list. A null or empty version returns all
     * runtimes.
     */
    public List<RuntimeInfo> locateVersion(final String version) {
        final RuntimeVersion vsn = new RuntimeVersion(version, null);
        return locateVersion(vsn);
    }

    public List<RuntimeInfo> locateVersion(final RuntimeVersion vsn) {
        final List<RuntimeInfo> result = new ArrayList<RuntimeInfo>();
        for (final RuntimeInfo info : getRuntimes()) {
            final RuntimeVersion v = info.getVersion();
            if (v.isReleaseCompatible(vsn)) {
                result.add(info);
            }
        }
        Collections.reverse(result);
        // add even newer versions, but at the end
        for (final RuntimeInfo info : getRuntimes()) {
            final RuntimeVersion v = info.getVersion();
            if (!result.contains(info) && v.compareTo(vsn) > 0) {
                result.add(info);
            }
        }
        return result;
    }

    public RuntimeInfo getRuntime(final RuntimeVersion runtimeVersion,
            final String runtimeName) {
        final List<RuntimeInfo> vsns = locateVersion(runtimeVersion);
        if (vsns.size() == 0) {
            return null;
        } else if (vsns.size() == 1) {
            return vsns.get(0);
        } else {
            for (final RuntimeInfo ri : vsns) {
                if (ri.getName().equals(runtimeName)) {
                    return ri;
                }
            }
            return vsns.get(0);
        }
    }

    public String[][] getAllRuntimesVersions() {
        final Collection<RuntimeInfo> rs = getRuntimes();
        final String[][] runtimes = new String[rs.size()][2];
        final Iterator<RuntimeInfo> it = rs.iterator();
        for (int i = 0; i < rs.size(); i++) {
            runtimes[i][0] = it.next().getVersion().asMinor().toString();
            runtimes[i][1] = runtimes[i][0];
        }
        return runtimes;
    }

    /**
     * If runtime is not set, try to locate one. The first one found as below is
     * set as default. All "obvious" runtimes found are stored.
     * <ul>
     * <li>A system property <code>erlide.runtime</code> can be set to point to
     * a location.</li>
     * <li>A preference in the default scope
     * <code>org.erlide.core/default_runtime</code> can be set to point to a
     * location.</li>
     * <li>Look for existing Erlang runtimes in a few obvious places and install
     * them, choosing a suitable one as default.</li>
     * </ul>
     * 
     */
    public void initializeRuntimesList() {
        final Collection<RuntimeInfo> found = guessRuntimeLocations();
        for (final RuntimeInfo info : found) {
            addRuntime(info);
        }
    }

    private void setDefaultRuntimes() {
        final List<RuntimeInfo> list = new ArrayList<RuntimeInfo>(getRuntimes());
        Collections.sort(list, new Comparator<RuntimeInfo>() {
            @Override
            public int compare(final RuntimeInfo o1, final RuntimeInfo o2) {
                final int x = o2.getVersion().compareTo(o1.getVersion());
                if (x != 0) {
                    return x;
                }
                return o2.getName().compareTo(o1.getName());
            }
        });
        if (list.size() > 0) {
            final String firstName = list.get(0).getName();
            if (defaultRuntimeName == null) {
                setDefaultRuntime(firstName);
            }

            // the erlide backend is the most recent stable version
            for (final RuntimeInfo info : list) {
                if (info.getVersion().isStable()) {
                    setErlideRuntime(info);
                    break;
                }
            }
            if (erlideRuntime == null) {
                setErlideRuntime(getDefaultRuntime());
            }
        }
    }

    private Collection<RuntimeInfo> guessRuntimeLocations() {
        final List<RuntimeInfo> result = Lists.newArrayList();
        final String[] locations = {
                System.getProperty("erlide.runtime"),
                new DefaultScope().getNode("org.erlide.core").get(
                        "default_runtime", null), "c:/program files",
                "c:/program files (x86)", "c:/programs", "c:/", "c:/apps",
                System.getProperty("user.home"), "/usr", "/usr/lib",
                "/usr/local", "/usr/local/lib",
                "/Library/Frameworks/erlang/Versions" };
        for (final String loc : locations) {
            final Collection<File> roots = findRuntime(loc);
            for (final File root : roots) {
                final RuntimeInfo rt = new RuntimeInfo();
                rt.setOtpHome(root.getPath());
                rt.setName(root.getName());
                result.add(rt);
            }
        }
        return result;
    }

    private static Collection<File> findRuntime(final String loc) {
        final Collection<File> result = new ArrayList<File>();
        if (loc == null) {
            return result;
        }
        final File folder = new File(loc);
        if (!folder.exists()) {
            return result;
        }
        final File[] candidates = folder.listFiles(new FileFilter() {
            @Override
            public boolean accept(final File pathname) {
                final String path = pathname.getName();
                return pathname.isDirectory()
                        && (path.startsWith("otp") || path.startsWith("erl")
                                || path.startsWith("Erl") || path
                                    .startsWith("R"));
            }
        });
        for (final File f : candidates) {
            if (RuntimeInfo.validateLocation(f.getPath())) {
                result.add(f);
            }
        }
        return result;
    }

}
