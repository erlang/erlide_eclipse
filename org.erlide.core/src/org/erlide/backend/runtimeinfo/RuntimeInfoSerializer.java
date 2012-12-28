package org.erlide.backend.runtimeinfo;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.erlide.core.ErlangCore;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.utils.ErlLogger;
import org.erlide.utils.PreferencesUtils;
import org.osgi.service.prefs.BackingStoreException;

public class RuntimeInfoSerializer implements IRuntimeInfoSerializer {

    private RuntimeInfoManager owner;

    @Override
    public void setOwner(final RuntimeInfoManager runtimeInfoManager) {
        owner = runtimeInfoManager;
        owner.getRootPreferenceNode().addPreferenceChangeListener(this);
    }

    @Override
    public synchronized void store() {
        IEclipsePreferences root = owner.getRootPreferenceNode();
        try {
            root.removePreferenceChangeListener(this);
            root.removeNode();
            root = owner.getRootPreferenceNode();

            for (final RuntimeInfo rt : owner.getRuntimes()) {
                final RuntimeInfoLoader rtl = new RuntimeInfoLoader(rt);
                rtl.store(root);
            }
            if (owner.getDefaultRuntimeName() != null) {
                root.put("default", owner.getDefaultRuntimeName());
            }
            if (owner.getErlideRuntime() != null) {
                root.put("erlide", owner.getErlideRuntime().getName());
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

    @Override
    public synchronized void load() {
        owner.fRuntimes.clear();
        loadDefaultPrefs();

        IEclipsePreferences root = new DefaultScope()
                .getNode(ErlangCore.PLUGIN_ID + "/runtimes");
        loadPrefs(root);
        root = owner.getRootPreferenceNode();
        loadPrefs(root);
    }

    private void loadDefaultPrefs() {
        final IPreferencesService ps = Platform.getPreferencesService();
        final String DEFAULT_ID = "org.erlide";

        final String defName = ps.getString(DEFAULT_ID, "default_name", null,
                null);
        final RuntimeInfo runtime = owner.getRuntime(defName);
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
            owner.addRuntime(rt);
        }
        owner.defaultRuntimeName = defName;
    }

    private void loadPrefs(final IEclipsePreferences root) {
        final String defrt = root.get("default", null);
        if (defrt != null) {
            owner.defaultRuntimeName = defrt;
        }

        String[] children;
        try {
            children = root.childrenNames();
            for (final String name : children) {
                final RuntimeInfo rt = new RuntimeInfo();
                final RuntimeInfoLoader rtl = new RuntimeInfoLoader(rt);
                rtl.load(root.node(name));
                owner.fRuntimes.put(name, rt);
            }
        } catch (final BackingStoreException e) {
            ErlLogger.warn(e);
        }

        if (owner.getDefaultRuntime() == null) {
            if (owner.defaultRuntimeName == null && owner.fRuntimes.size() > 0) {
                owner.defaultRuntimeName = owner.fRuntimes.values().iterator()
                        .next().getName();
            }
        }
    }

    @Override
    public void preferenceChange(final PreferenceChangeEvent event) {
        if (event.getNode().absolutePath().contains("org.erlide")) {
            load();
        }
    }

}
