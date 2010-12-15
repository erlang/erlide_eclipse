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
package org.erlide.runtime.backend;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
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
import org.erlide.core.ErlangPlugin;
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.backend.RuntimeInfoListener;
import org.erlide.jinterface.backend.RuntimeVersion;
import org.erlide.jinterface.backend.util.PreferencesUtils;
import org.erlide.jinterface.util.ErlLogger;
import org.osgi.service.prefs.BackingStoreException;

public final class RuntimeInfoManager implements IPreferenceChangeListener {

    private RuntimeInfo erlideRuntime;

    private RuntimeInfoManager() {
        getRootPreferenceNode().addPreferenceChangeListener(this);
        load();
    }

    @SuppressWarnings("synthetic-access")
    private static class LazyRuntimeInfoManagerHolder {
        public static final RuntimeInfoManager instance = new RuntimeInfoManager();
    }

    public static synchronized RuntimeInfoManager getDefault() {
        return LazyRuntimeInfoManagerHolder.instance;
    }

    private final Map<String, RuntimeInfo> fRuntimes = new HashMap<String, RuntimeInfo>();
    private String defaultRuntimeName = "";

    private final List<RuntimeInfoListener> fListeners = new ArrayList<RuntimeInfoListener>();

    public Collection<RuntimeInfo> getRuntimes() {
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
                .getNode(ErlangPlugin.PLUGIN_ID + "/runtimes");
        loadPrefs(root);
        root = getRootPreferenceNode();
        loadPrefs(root);
    }

    private synchronized void loadDefaultPrefs() {
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
            final String wd = ps.getString(DEFAULT_ID, "default_"
                    + RuntimeInfoLoader.WORKING_DIR, "", null);
            if (wd.length() != 0) {
                rt.setWorkingDir(wd);
            }
            rt.setManaged(ps.getBoolean(DEFAULT_ID, "default_"
                    + RuntimeInfoLoader.MANAGED, true, null));
            addRuntime(rt);
        }
        defaultRuntimeName = defName;
    }

    private synchronized void loadPrefs(final IEclipsePreferences root) {
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
        final RuntimeInfo rt = getRuntime(root.get("erlide", null));
        setErlideRuntime(rt == null ? getDefaultRuntime() : rt);
    }

    protected IEclipsePreferences getRootPreferenceNode() {
        return new InstanceScope()
                .getNode(ErlangPlugin.PLUGIN_ID + "/runtimes");
    }

    public void setRuntimes(final Collection<RuntimeInfo> elements) {
        fRuntimes.clear();
        for (final RuntimeInfo rt : elements) {
            fRuntimes.put(rt.getName(), rt);
        }
        notifyListeners();
    }

    public void addRuntime(final RuntimeInfo rt) {
        if (!fRuntimes.containsKey(rt.getName())) {
            fRuntimes.put(rt.getName(), rt);
        }
        notifyListeners();
    }

    public Collection<String> getRuntimeNames() {
        return fRuntimes.keySet();
    }

    public boolean isDuplicateName(final String name) {
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

    public void removeRuntime(final String name) {
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

    public synchronized void setErlideRuntime(final RuntimeInfo runtime) {
        if (runtime != null) {
            runtime.setNodeName("erlide");
        }
        final RuntimeInfo old = erlideRuntime;
        if (old == null || !old.equals(runtime)) {
            erlideRuntime = runtime;
            notifyListeners();
            // this creates infinite recursion!
            // BackendManagerImpl.getDefault().getIdeBackend().stop();
        }
    }

    public synchronized RuntimeInfo getErlideRuntime() {
        if (erlideRuntime == null) {
            RuntimeInfo ri = null;
            final Iterator<RuntimeInfo> iterator = getRuntimes().iterator();
            if (iterator.hasNext()) {
                ri = iterator.next();
            }
            if (ri != null) {
                setErlideRuntime(ri);
            } else {
                ErlLogger.error("There is no erlideRuntime defined!");
            }
        }
        return erlideRuntime;
    }

    public synchronized RuntimeInfo getDefaultRuntime() {
        return getRuntime(getDefaultRuntimeName());
    }

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
}
