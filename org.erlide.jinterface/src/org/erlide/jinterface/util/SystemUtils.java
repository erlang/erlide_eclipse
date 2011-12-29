package org.erlide.jinterface.util;

public class SystemUtils {
    private static Boolean fgCacheIsEricssonUser;

    public static boolean isDeveloper() {
        final String dev = System.getProperty("erlide.devel");
        return dev != null && Boolean.parseBoolean(dev);
    }

    public static boolean isClearCacheAvailable() {
        final String test = System.getProperty("erlide.clearCacheAvailable");
        return test != null && Boolean.parseBoolean(test);
    }

    public static boolean isEricssonUser() {
        if (fgCacheIsEricssonUser == null) {
            final String dev = System.getProperty("erlide.ericsson.user");
            fgCacheIsEricssonUser = Boolean.valueOf(dev);
        }
        return fgCacheIsEricssonUser.booleanValue();
    }

    public static boolean isTest() {
        final String test = System.getProperty("erlide.test");
        return test != null && Boolean.parseBoolean(test);
    }

    public static boolean isOnWindows() {
        return System.getProperty("os.name").toLowerCase().contains("windows");
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

    public static boolean hasFeatureEnabled(final String feature) {
        return Boolean.parseBoolean(System.getProperty(feature));
    }

}
