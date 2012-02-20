package org.erlide.core.internal.model.erlang;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.ConfigurationScope;
import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.osgi.service.prefs.BackingStoreException;

public final class PreferencesHelper {

    private final IScopeContext[] loadContexts;
    private final IScopeContext storeContext;
    private final IPreferencesService service;
    private final String qualifier;
    private final IScopeContext[] nextContexts;

    public static PreferencesHelper getHelper(final String qualifier,
            final IProject project) {
        return new PreferencesHelper(qualifier, new IScopeContext[] {
                new ProjectScope(project), new InstanceScope(),
                new ConfigurationScope(), new DefaultScope() },
                new ProjectScope(project));
    }

    public static PreferencesHelper getHelper(final String qualifier) {
        final InstanceScope instanceScope = new InstanceScope();
        return new PreferencesHelper(qualifier, new IScopeContext[] {
                instanceScope, new ConfigurationScope(), new DefaultScope() },
                instanceScope);
    }

    private PreferencesHelper(final String qualifier,
            final IScopeContext[] loadContexts, final IScopeContext storeContext) {
        this.loadContexts = loadContexts;
        this.storeContext = storeContext;
        nextContexts = getNextContexts(loadContexts, storeContext);
        service = Platform.getPreferencesService();
        this.qualifier = qualifier;
    }

    public boolean getBoolean(final String key, final boolean defaultValue) {
        return service.getBoolean(qualifier, key, defaultValue, loadContexts);
    }

    public byte[] getByteArray(final String key, final byte[] defaultValue) {
        return service.getByteArray(qualifier, key, null, loadContexts);
    }

    public double getDouble(final String key, final double defaultValue) {
        return service.getDouble(qualifier, key, defaultValue, loadContexts);
    }

    public float getFloat(final String key, final float defaultValue) {
        return service.getFloat(qualifier, key, defaultValue, loadContexts);
    }

    public int getInt(final String key, final int defaultValue) {
        return service.getInt(qualifier, key, defaultValue, loadContexts);
    }

    public long getLong(final String key, final long defaultValue) {
        return service.getLong(qualifier, key, defaultValue, loadContexts);
    }

    public String getString(final String key, final String defaultValue) {
        return service.getString(qualifier, key, defaultValue, loadContexts);
    }

    public void putBoolean(final String key, final boolean value) {
        final String def = service
                .getString(qualifier, key, null, nextContexts);
        if (def == null || Boolean.parseBoolean(def) != value) {
            storeContext.getNode(qualifier).putBoolean(key, value);
        }
    }

    public static IScopeContext[] getNextContexts(final IScopeContext[] list,
            final IScopeContext item) {
        final List<IScopeContext> result = new ArrayList<IScopeContext>();
        boolean found = false;
        for (final IScopeContext ctx : list) {
            if (found) {
                result.add(ctx);
            }
            if (ctx.equals(item)) {
                found = true;
            }
        }
        return result.toArray(new IScopeContext[0]);
    }

    public void putByteArray(final String key, final byte[] value) {
        final byte[] def = service.getByteArray(qualifier, key, null,
                nextContexts);
        if (def == null || def != value) {
            storeContext.getNode(qualifier).putByteArray(key, value);
        }
    }

    public void putDouble(final String key, final double value) {
        final double def = service.getDouble(qualifier, key, Double.NaN,
                nextContexts);
        if (Double.isNaN(def) || Double.compare(def, value) != 0) {
            storeContext.getNode(qualifier).putDouble(key, value);
        }
    }

    public void putFloat(final String key, final float value) {
        final float def = service.getFloat(qualifier, key, Float.NaN,
                nextContexts);
        if (Double.isNaN(def) || def != value) {
            storeContext.getNode(qualifier).putFloat(key, value);
        }
    }

    public void putInt(final String key, final int value) {
        final int def = service.getInt(qualifier, key, Integer.MIN_VALUE,
                nextContexts);
        if (def == Integer.MIN_VALUE || Double.compare(def, value) != 0) {
            storeContext.getNode(qualifier).putInt(key, value);
        }
    }

    public void putLong(final String key, final long value) {
        final long def = service.getLong(qualifier, key, Long.MIN_VALUE,
                nextContexts);
        if (def == Long.MIN_VALUE || def != value) {
            storeContext.getNode(qualifier).putLong(key, value);
        }
    }

    public void putString(final String key, final String value) {
        final String def = service
                .getString(qualifier, key, null, nextContexts);
        if (value == null) {
            storeContext.getNode(qualifier).remove(key);
        } else if (def == null || !def.equals(value)) {
            storeContext.getNode(qualifier).put(key, value);
        }
    }

    public void remove(final String key, final IScopeContext scope) {
        scope.getNode(qualifier).remove(key);
    }

    public void remove(final String key) {
        remove(key, storeContext);
    }

    public void removeNode(final String key, final IScopeContext scope)
            throws BackingStoreException {
        scope.getNode(qualifier + "/" + key).removeNode();
    }

    public void removeNode(final String key) throws BackingStoreException {
        remove(key, storeContext);
    }

    public void flush() throws BackingStoreException {
        storeContext.getNode(qualifier).flush();
    }

    public boolean hasAnyAtLowestScope() {
        final IScopeContext sc = storeContext;
        final IEclipsePreferences p = sc.getNode(qualifier);
        if (p != null) {
            try {
                final String[] keys = p.keys();
                return keys.length > 0;
            } catch (final BackingStoreException e) {
            }
        }
        return false;
    }

    public void removeAllAtLowestScope() {
        final IScopeContext sc = storeContext;
        final IEclipsePreferences p = sc.getNode(qualifier);
        if (p != null) {
            try {
                final String[] keys = p.keys();
                for (final String key : keys) {
                    remove(key, sc);
                }
                flush();
            } catch (final BackingStoreException e) {
            }
        }
    }
}
