package org.erlide.tracing.core.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Collections;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Plugin;
import org.erlide.jinterface.ErlLogger;
import org.erlide.tracing.core.Activator;
import org.erlide.tracing.core.TraceBackend;
import org.erlide.tracing.core.mvc.model.TracePattern;
import org.erlide.tracing.core.mvc.model.TracedNode;

/**
 * Class containing helper methods for managing nodes and trace patterns
 * configurations. It loads loads and saves data in plugins directory (path to
 * this directory is obtained using {@link Plugin#getStateLocation()}).
 * 
 * @author Piotr Dorobisz
 * 
 */
public final class ConfigurationManager {

    private static String TP_DIR = "trace_patterns";
    private static String NODES_DIR = "nodes";

    private ConfigurationManager() {
    }

    /**
     * Loads trace patterns configuration.
     * 
     * @param configName
     *            configuration name
     * @return loaded trace patterns
     */
    public static TracePattern[] loadTPConfig(final String configName) {
        final ArrayList<TracePattern> patterns = new ArrayList<TracePattern>();

        final Object[] objects = loadConfiguration(configName, TP_DIR);
        if (objects != null) {
            for (final Object object : objects) {
                patterns.add((TracePattern) object);
            }
        }
        return patterns.toArray(new TracePattern[patterns.size()]);
    }

    /**
     * Loads nodes configuration.
     * 
     * @param configName
     *            configuration name
     * @return loaded nodes
     */
    public static TracedNode[] loadNodesConfig(final String configName) {
        final ArrayList<TracedNode> nodes = new ArrayList<TracedNode>();

        final Object[] objects = loadConfiguration(configName, NODES_DIR);
        if (objects != null) {
            for (final Object object : objects) {
                nodes.add((TracedNode) object);
            }
        }
        return nodes.toArray(new TracedNode[nodes.size()]);
    }

    /**
     * Saves trace patterns configuration under given name. If configuration
     * with this name does not exist it will be created. Otherwise it will be
     * overwritten.
     * 
     * @param configName
     * @return <code>true</code> if configuration was saved, <code>false</code>
     *         otherwise
     */
    public static boolean saveTPConfig(final String configName) {
        return saveConfiguration(configName, TP_DIR, TraceBackend.getInstance()
                .getTracePatternsArray());
    }

    /**
     * Saves nodes configuration under given name. If configuration with this
     * name does not exist it will be created. Otherwise it will be overwritten.
     * 
     * @param configName
     * @return <code>true</code> if configuration was saved, <code>false</code>
     *         otherwise
     */
    public static boolean saveNodesConfig(final String configName) {
        return saveConfiguration(configName, NODES_DIR, TraceBackend
                .getInstance().getTracedNodesArray());
    }

    /**
     * Deletes trace pattern configuration.
     * 
     * @param configName
     *            configuration name
     * @return <code>true</code> if configuration was deleted,
     *         <code>false</code> otherwise
     */
    public static boolean removeTPConfig(final String configName) {
        return removeConfiguration(configName, TP_DIR);
    }

    /**
     * Deletes nodes configuration.
     * 
     * @param configName
     *            configuration name
     * @return <code>true</code> if configuration was deleted,
     *         <code>false</code> otherwise
     */
    public static boolean removeNodesConfig(final String configName) {
        return removeConfiguration(configName, NODES_DIR);
    }

    /**
     * Returns list of all available trace patterns configurations.
     * 
     * @return list of trace pattern configuration names
     */
    public static String[] getTPConfigs() {
        return getConfigurationsList(TP_DIR);
    }

    /**
     * Returns list of all available nodes configurations.
     * 
     * @return list of trace pattern configuration names
     */
    public static String[] getNodesConfig() {
        return getConfigurationsList(NODES_DIR);
    }

    private static Object[] loadConfiguration(final String configName,
            final String dirName) {

        final IPath location = Activator.getDefault().getStateLocation()
                .append(dirName).append(configName);
        final File file = location.toFile();

        if (file.exists() && file.isFile()) {
            ObjectInputStream objectInputStream = null;
            try {
                final FileInputStream inputStream = new FileInputStream(file);
                objectInputStream = new ObjectInputStream(inputStream);
                return (Object[]) objectInputStream.readObject();

            } catch (final Exception e) {
                ErlLogger.error(e);
            } finally {
                if (objectInputStream != null) {
                    try {
                        objectInputStream.close();
                    } catch (final IOException e) {
                        ErlLogger.error(e);
                    }
                }
            }
        }
        return null;
    }

    private static boolean saveConfiguration(final String configName,
            final String dirName, final Object configuration) {
        ObjectOutputStream objectOutputStream = null;
        try {
            final IPath location = Activator.getDefault().getStateLocation()
                    .append(dirName);
            final File dir = location.toFile();

            if (!dir.exists() && !dir.mkdir()) {
                return false;
            }

            final FileOutputStream out = new FileOutputStream(location.append(
                    configName).toFile());
            objectOutputStream = new ObjectOutputStream(out);
            objectOutputStream.writeObject(configuration);
            return true;
        } catch (final Exception e) {
            ErlLogger.error(e);
        } finally {
            if (objectOutputStream != null) {
                try {
                    objectOutputStream.close();
                } catch (final IOException e) {
                    ErlLogger.error(e);
                }
            }
        }
        return false;
    }

    private static boolean removeConfiguration(final String configName,
            final String dirName) {
        final IPath location = Activator.getDefault().getStateLocation()
                .append(dirName).append(configName);
        final File file = location.toFile();
        if (file.exists() && file.isFile()) {
            return file.delete();
        } else {
            return false;
        }
    }

    private static String[] getConfigurationsList(final String dirName) {
        final ArrayList<String> configNames = new ArrayList<String>();

        final IPath location = Activator.getDefault().getStateLocation()
                .append(dirName);
        final File dir = location.toFile();
        if (dir.exists()) {
            for (final File file : dir.listFiles()) {
                if (file.isFile()) {
                    configNames.add(file.getName());
                }
            }
        }
        Collections.sort(configNames);
        return configNames.toArray(new String[configNames.size()]);
    }
}
