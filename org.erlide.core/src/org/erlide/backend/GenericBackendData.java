package org.erlide.backend;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.erlide.jinterface.ErlLogger;
import org.erlide.launch.ErlangLaunchDelegate;

/**
 * @noextend This class is not intended to be subclassed by clients.
 * @noinstantiate This class is not intended to be instantiated by clients.
 */
public class GenericBackendData {

    protected ILaunch launch;
    protected ILaunchConfigurationWorkingCopy config;
    boolean debug = false;

    public GenericBackendData(final ILaunchConfiguration aConfig,
            final String mode) {
        debug = mode.equals(ILaunchManager.DEBUG_MODE);
        try {
            if (aConfig != null) {
                config = aConfig.getWorkingCopy();
            } else {
                final ILaunchManager manager = DebugPlugin.getDefault()
                        .getLaunchManager();
                final ILaunchConfigurationType ltype = manager
                        .getLaunchConfigurationType(ErlangLaunchDelegate.CONFIGURATION_TYPE_INTERNAL);
                config = ltype.newInstance(null,
                        manager.generateLaunchConfigurationName("erlide"));
            }
            launch = null;
        } catch (final CoreException e) {
            ErlLogger.error(e);
            config = null;
        }
    }

    @SuppressWarnings("rawtypes")
    public void debugPrint() {
        final String mode = debug ? "debug" : "run";
        ErlLogger.info("Backend data:" + mode + " mode,  with attributes::");
        Map attrs;
        try {
            attrs = config.getAttributes();
            for (final Object oe : attrs.entrySet()) {
                final Entry e = (Entry) oe;
                String key = (String) e.getKey();
                if (key.startsWith("org.erlide.core.")) {
                    key = "*." + key.substring(15);
                }
                ErlLogger.info("%-30s: %s", key, e.getValue());
            }
        } catch (final CoreException e1) {
            ErlLogger.info("Could not get attributes! %s", e1.getMessage());
        }
        ErlLogger.info("---------------");
    }

    public String getStringAttribute(final String key, final String defaultValue) {
        try {
            return config.getAttribute(key, defaultValue).trim();
        } catch (final CoreException e) {
            e.printStackTrace();
            return defaultValue;
        }
    }

    public boolean getBooleanAttribute(final String key,
            final boolean defaultValue) {
        try {
            return config.getAttribute(key, defaultValue);
        } catch (final CoreException e) {
            e.printStackTrace();
            return defaultValue;
        }
    }

    public int getIntAttribute(final String key, final int defaultValue) {
        try {
            return config.getAttribute(key, defaultValue);
        } catch (final CoreException e) {
            e.printStackTrace();
            return defaultValue;
        }
    }

    @SuppressWarnings("unchecked")
    public Map<String, String> getMapAttribute(final String key,
            final Map<String, String> defaultValue) {
        try {
            return config.getAttribute(key, defaultValue);
        } catch (final CoreException e) {
            e.printStackTrace();
            return defaultValue;
        }
    }

    @SuppressWarnings("unchecked")
    public List<String> getListAttribute(final String key,
            final List<String> defaultValue) {
        try {
            return config.getAttribute(key, defaultValue);
        } catch (final CoreException e) {
            e.printStackTrace();
            return defaultValue;
        }
    }

    public void setLaunch(final ILaunch launch) {
        this.launch = launch;
    }

    public void setAttribute(final String key, final String value) {
        config.setAttribute(key, value);
    }

    public void setAttribute(final String key, final boolean value) {
        config.setAttribute(key, value);
    }

    public void setAttribute(final String key, final int value) {
        config.setAttribute(key, value);
    }

    public void setAttribute(final String key, final Map<String, String> value) {
        config.setAttribute(key, value);
    }

}
