package org.ttb.integration.mvc.model;

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
import org.erlide.jinterface.util.ErlLogger;
import org.ttb.integration.Activator;
import org.ttb.integration.TtbBackend;

/**
 * Class containing helper methods for managing trace patterns configurations.
 * It loads loads and saves data in plugins directory (path to this directory is
 * obtained using {@link Plugin#getStateLocation()}).
 * 
 * @author Piotr Dorobisz
 * 
 */
public final class ConfigurationManager {

    private static String TP_DIR = "trace_patterns";

    private ConfigurationManager() {
    }

    /**
     * Loads trace patterns from given configuration
     * 
     * @param configName
     *            configuration name
     * @return
     */
    public static TracePattern[] loadTracePatterns(String configName) {
        ArrayList<TracePattern> patterns = new ArrayList<TracePattern>();

        IPath location = Activator.getDefault().getStateLocation().append(TP_DIR).append(configName);
        File file = location.toFile();

        if (file.exists() && file.isFile()) {
            ObjectInputStream objectInputStream = null;
            try {
                FileInputStream inputStream = new FileInputStream(file);
                objectInputStream = new ObjectInputStream(inputStream);
                Object[] objects = (Object[]) objectInputStream.readObject();
                for (Object object : objects) {
                    patterns.add((TracePattern) object);
                }
            } catch (Exception e) {
                ErlLogger.error(e);
            } finally {
                if (objectInputStream != null) {
                    try {
                        objectInputStream.close();
                    } catch (IOException e) {
                        ErlLogger.error(e);
                    }
                }
            }
        }
        return patterns.toArray(new TracePattern[patterns.size()]);
    }

    /**
     * Saves trace patterns in configuration with given name. If configuration
     * with this name does not exist it will be created. Otherwise it will be
     * overwritten.
     * 
     * @param configName
     * @return
     */
    public static boolean saveTracePatterns(String configName) {
        ObjectOutputStream objectOutputStream = null;
        try {
            IPath location = Activator.getDefault().getStateLocation().append(TP_DIR);
            File dir = location.toFile();

            if (!dir.exists() && !dir.mkdir()) {
                return false;
            }

            FileOutputStream out = new FileOutputStream(location.append(configName).toFile());
            objectOutputStream = new ObjectOutputStream(out);
            objectOutputStream.writeObject(TtbBackend.getInstance().getTracePatternsArray());
            return true;
        } catch (Exception e) {
            ErlLogger.error(e);
        } finally {
            if (objectOutputStream != null) {
                try {
                    objectOutputStream.close();
                } catch (IOException e) {
                    ErlLogger.error(e);
                }
            }
        }
        return false;
    }

    /**
     * Deletes trace pattern configuration.
     * 
     * @param configName
     *            configuration name
     * @return
     */
    public static boolean removeTracingPatterns(String configName) {
        IPath location = Activator.getDefault().getStateLocation().append(TP_DIR).append(configName);
        File file = location.toFile();
        if (file.exists() && file.isFile()) {
            return file.delete();
        } else
            return false;
    }

    /**
     * Returns list of all available trace patterns configurations.
     * 
     * @return list of trace pattern configuration names
     */
    public static String[] getTracePatternsConfigurations() {
        ArrayList<String> configNames = new ArrayList<String>();

        IPath location = Activator.getDefault().getStateLocation().append(TP_DIR);
        File dir = location.toFile();
        if (dir.exists()) {
            for (File file : dir.listFiles()) {
                if (file.isFile())
                    configNames.add(file.getName());
            }
        }
        Collections.sort(configNames);
        return configNames.toArray(new String[configNames.size()]);
    }
}
