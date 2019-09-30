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

    private static String rootKey = "org.erlide.backend" + "/runtimes";

    public static IEclipsePreferences getInstanceRootNode() {
        return InstanceScope.INSTANCE.getNode(RuntimeInfoPreferencesSerializer.rootKey);
    }

    public static IEclipsePreferences getDefaultRootNode() {
        return DefaultScope.INSTANCE.getNode(RuntimeInfoPreferencesSerializer.rootKey);
    }

    private final IEclipsePreferences defaultRootNode;
    private final IEclipsePreferences instanceRootNode;

    public RuntimeInfoPreferencesSerializer(final IEclipsePreferences instanceRootNode,
            final IEclipsePreferences defaultRootNode) {
        this.instanceRootNode = instanceRootNode;
        this.defaultRootNode = defaultRootNode;
    }

    public RuntimeInfoPreferencesSerializer() {
        this(RuntimeInfoPreferencesSerializer.getInstanceRootNode(),
                RuntimeInfoPreferencesSerializer.getDefaultRootNode());
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
                instanceRootNode.put(RuntimeInfoPreferencesSerializer.DEFAULT_KEY,
                        data.defaultRuntimeName);
            }
            if (data.erlideRuntimeName != null) {
                instanceRootNode.put(RuntimeInfoPreferencesSerializer.ERLIDE_KEY,
                        data.erlideRuntimeName);
            }
            instanceRootNode.flush();
        } catch (final Exception e) {
            ErlLogger.warn(e);
        }
    }

    @Override
    public synchronized RuntimeInfoCatalogData load() {
        RuntimeInfoCatalogData data = new RuntimeInfoCatalogData();
        data = loadPrefs(data, defaultRootNode);
        data = loadPrefs(data, instanceRootNode);

        String dflt = null;
        String ide = null;
        if (!data.runtimes.isEmpty()) {
            dflt = data.defaultRuntimeName != null ? data.defaultRuntimeName
                    : data.runtimes.iterator().next().getName();
            ide = data.erlideRuntimeName != null ? data.erlideRuntimeName : dflt;
        }
        return new RuntimeInfoCatalogData(data.runtimes, dflt, ide);
    }

    private RuntimeInfoCatalogData loadPrefs(final RuntimeInfoCatalogData data,
            final IEclipsePreferences root) {
        String[] children;
        final Collection<RuntimeInfo> runtimes = new ArrayList<>(data.runtimes);
        try {
            children = root.childrenNames();
            for (final String name : children) {
                final RuntimeInfo rt = RuntimeInfoLoader.load(root.node(name));
                runtimes.add(rt);
            }
        } catch (final BackingStoreException e) {
            ErlLogger.warn(e);
        }

        final String defaultRuntimeName = root.get(
                RuntimeInfoPreferencesSerializer.DEFAULT_KEY, data.defaultRuntimeName);
        final String ideRuntimeName = root
                .get(RuntimeInfoPreferencesSerializer.ERLIDE_KEY, data.erlideRuntimeName);
        return new RuntimeInfoCatalogData(runtimes, defaultRuntimeName, ideRuntimeName);
    }
}
