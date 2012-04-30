package org.erlide.utils;

public class SystemUtils {

    private static SystemUtils instance = new SystemUtils();

    public static SystemUtils getInstance() {
        return instance;
    }

    private final boolean mustDefineTclLib;
    private boolean developer;
    private boolean clearCacheAvailable;
    private boolean test;
    private final boolean onWindows;
    private boolean monitoringIdeBackend;
    private int monitoringInterval = 300;

    private SystemUtils() {
        mustDefineTclLib = hasFeatureEnabled("erlide.ericsson.user");
        developer = hasFeatureEnabled("erlide.devel");
        test = hasFeatureEnabled("erlide.test");
        clearCacheAvailable = hasFeatureEnabled("erlide.clearCacheAvailable");
        onWindows = System.getProperty("os.name").toLowerCase()
                .contains("windows");
        monitoringIdeBackend = hasFeatureEnabled("erlide.monitor.ide");
    }

    public boolean isDeveloper() {
        return developer;
    }

    public void setDeveloper(final boolean developer) {
        this.developer = developer;
    }

    public boolean isClearCacheAvailable() {
        return clearCacheAvailable;
    }

    public void setClearCacheAvailable(final boolean clearCacheAvailable) {
        this.clearCacheAvailable = clearCacheAvailable;
    }

    public boolean hasSpecialTclLib() {
        return mustDefineTclLib;
    }

    public boolean isTest() {
        return test;
    }

    public void setTest(final boolean test) {
        this.test = test;
    }

    public boolean isOnWindows() {
        return onWindows;
    }

    public static boolean hasExtension(final String name) {
        final int i = name.lastIndexOf('.');
        return i != -1;
    }

    public static String withoutExtension(final String name) {
        final int i = name.lastIndexOf('.');
        if (i == -1) {
            return name;
        }
        return name.substring(0, i);
    }

    // only to be used internally
    @Deprecated
    public static boolean hasFeatureEnabled(final String feature) {
        return Boolean.parseBoolean(System.getProperty(feature));
    }

    public boolean isMonitoringIdeBackend() {
        return monitoringIdeBackend;
    }

    public void setMonitoringIdeBackend(final boolean value) {
        monitoringIdeBackend = value;
    }

    public int getMonitoringInterval() {
        return monitoringInterval;
    }

    public void setMonitoringInterval(final int value) {
        monitoringInterval = value;
    }

}
