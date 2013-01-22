package org.erlide.backend.runtimeinfo;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.erlide.core.ErlangCore;
import org.erlide.runtime.runtimeinfo.IRuntimeInfoSerializer;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeInfoManagerData;
import org.erlide.utils.ErlLogger;
import org.osgi.service.prefs.BackingStoreException;

public class RuntimeInfoPreferencesSerializer implements IRuntimeInfoSerializer {

    private static String rootKey = ErlangCore.PLUGIN_ID + "/runtimes";

    public static IEclipsePreferences getInstanceRootNode() {
        return new InstanceScope().getNode(rootKey);
    }

    public static IEclipsePreferences getDefaultRootNode() {
        return new DefaultScope().getNode(rootKey);
    }

    private final IEclipsePreferences defaultRootNode;
    private final IEclipsePreferences instanceRootNode;

    public RuntimeInfoPreferencesSerializer(
            final IEclipsePreferences instanceRootNode,
            final IEclipsePreferences defaultRootNode) {
        this.instanceRootNode = instanceRootNode;
        this.defaultRootNode = defaultRootNode;
    }

    public RuntimeInfoPreferencesSerializer() {
        this(getInstanceRootNode(), getDefaultRootNode());
    }

    @Override
    public synchronized void store(final RuntimeInfoManagerData data) {
        try {
            instanceRootNode.clear();
            for (final String node : instanceRootNode.childrenNames()) {
                instanceRootNode.node(node).removeNode();
            }

            for (final RuntimeInfo rt : data.runtimes) {
                RuntimeInfoLoader.store(rt, instanceRootNode);
            }
            if (data.defaultRuntimeName != null) {
                instanceRootNode.put("default", data.defaultRuntimeName);
            }
            if (data.erlideRuntimeName != null) {
                instanceRootNode.put("erlide", data.erlideRuntimeName);
            }
            instanceRootNode.flush();
        } catch (final Exception e) {
            ErlLogger.warn(e);
        }
    }

    @Override
    public synchronized RuntimeInfoManagerData load() {
        RuntimeInfoManagerData data = new RuntimeInfoManagerData();
        data = loadPrefs(data, defaultRootNode);
        data = loadPrefs(data, instanceRootNode);

        return data;
    }

    private RuntimeInfoManagerData loadPrefs(final RuntimeInfoManagerData data,
            final IEclipsePreferences root) {
        final String defrt = root.get("default", null);
        String defaultRuntimeName = null;
        if (defrt != null) {
            defaultRuntimeName = defrt;
        }

        String[] children;
        final Collection<RuntimeInfo> runtimes = new ArrayList<RuntimeInfo>(
                data.runtimes);
        try {
            children = root.childrenNames();
            for (final String name : children) {
                final RuntimeInfo rt = RuntimeInfoLoader.load(root.node(name));
                runtimes.add(rt);
            }
        } catch (final BackingStoreException e) {
            ErlLogger.warn(e);
        }

        if (data.defaultRuntimeName == null && data.runtimes.size() > 0) {
            defaultRuntimeName = data.runtimes.iterator().next().getName();
        }
        return new RuntimeInfoManagerData(runtimes, defaultRuntimeName, null);
    }
}
