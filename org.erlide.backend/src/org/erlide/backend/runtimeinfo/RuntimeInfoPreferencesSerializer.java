package org.erlide.backend.runtimeinfo;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.erlide.runtime.runtimeinfo.IRuntimeInfoSerializer;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeInfoCatalogData;
import org.erlide.util.ErlLogger;
import org.osgi.service.prefs.BackingStoreException;

public class RuntimeInfoPreferencesSerializer implements IRuntimeInfoSerializer {

    private static final String ERLIDE_KEY = "erlide";
    private static final String DEFAULT_KEY = "default";

    // TODO remove old key in a few releases
    @Deprecated
    private static String rootKeyOld = "org.erlide.core" + "/runtimes";
    private static String rootKey = "org.erlide.backend" + "/runtimes";

    @Deprecated
    public static IEclipsePreferences getInstanceOldRootNode() {
        return InstanceScope.INSTANCE.getNode(rootKeyOld);
    }

    @Deprecated
    public static IEclipsePreferences getDefaultOldRootNode() {
        return DefaultScope.INSTANCE.getNode(rootKeyOld);
    }

    public static IEclipsePreferences getInstanceRootNode() {
        return InstanceScope.INSTANCE.getNode(rootKey);
    }

    public static IEclipsePreferences getDefaultRootNode() {
        return DefaultScope.INSTANCE.getNode(rootKey);
    }

    private final IEclipsePreferences defaultRootNode;
    private final IEclipsePreferences instanceRootNode;

    public RuntimeInfoPreferencesSerializer(final IEclipsePreferences instanceRootNode,
            final IEclipsePreferences defaultRootNode) {
        this.instanceRootNode = instanceRootNode;
        this.defaultRootNode = defaultRootNode;
    }

    public RuntimeInfoPreferencesSerializer() {
        this(getInstanceRootNode(), getDefaultRootNode());
    }

    @Override
    public synchronized void store(final RuntimeInfoCatalogData data) {
        try {
            instanceRootNode.clear();
            for (final String node : instanceRootNode.childrenNames()) {
                instanceRootNode.node(node).removeNode();
            }

            for (final RuntimeInfo rt : data.runtimes) {
                RuntimeInfoLoader.store(rt, instanceRootNode);
            }
            if (data.defaultRuntimeName != null) {
                instanceRootNode.put(DEFAULT_KEY, data.defaultRuntimeName);
            }
            if (data.erlideRuntimeName != null) {
                instanceRootNode.put(ERLIDE_KEY, data.erlideRuntimeName);
            }
            instanceRootNode.flush();
        } catch (final Exception e) {
            ErlLogger.warn(e);
        }
    }

    @Override
    public synchronized RuntimeInfoCatalogData load() {
        RuntimeInfoCatalogData data = new RuntimeInfoCatalogData();
        data = loadPrefs(data, getDefaultOldRootNode());
        data = loadPrefs(data, defaultRootNode);
        data = loadPrefs(data, getInstanceOldRootNode());
        data = loadPrefs(data, instanceRootNode);

        String dflt = null;
        String ide = null;
        if (data.runtimes.size() > 0) {
            dflt = data.defaultRuntimeName != null ? data.defaultRuntimeName
                    : data.runtimes.iterator().next().getName();
            ide = data.erlideRuntimeName != null ? data.erlideRuntimeName : dflt;
        }
        return new RuntimeInfoCatalogData(data.runtimes, dflt, ide);
    }

    private RuntimeInfoCatalogData loadPrefs(final RuntimeInfoCatalogData data,
            final IEclipsePreferences root) {
        String[] children;
        final Collection<RuntimeInfo> runtimes = new ArrayList<RuntimeInfo>(data.runtimes);
        try {
            children = root.childrenNames();
            for (final String name : children) {
                final RuntimeInfo rt = RuntimeInfoLoader.load(root.node(name));
                runtimes.add(rt);
            }
        } catch (final BackingStoreException e) {
            ErlLogger.warn(e);
        }

        final String defaultRuntimeName = root.get(DEFAULT_KEY, data.defaultRuntimeName);
        final String ideRuntimeName = root.get(ERLIDE_KEY, data.erlideRuntimeName);
        return new RuntimeInfoCatalogData(runtimes, defaultRuntimeName, ideRuntimeName);
    }
}
