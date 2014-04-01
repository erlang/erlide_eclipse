package org.erlide.util;

public final class SystemConfiguration {

    private static SystemConfiguration instance = new SystemConfiguration();

    public static SystemConfiguration getInstance() {
        return instance;
    }

    private final boolean mustDefineTclLib;
    private boolean developer;
    private boolean clearCacheAvailable;
    private boolean test;
    private final boolean onWindows;
    private int warnProcessSizeLimitMB;
    private int killProcessSizeLimitMB;
    private int maxParallelBuilds;
    private static final int MIN_WARN_LIMIT = 5;
    private static final int MIN_KILL_LIMIT = 10;

    private SystemConfiguration() {
        mustDefineTclLib = hasFeatureEnabled("erlide.ericsson.user");
        developer = hasFeatureEnabled("erlide.devel");
        test = hasFeatureEnabled("erlide.test");
        clearCacheAvailable = hasFeatureEnabled("erlide.clearCacheAvailable");
        onWindows = System.getProperty("os.name").toLowerCase().contains("windows");
        setWarnProcessSizeLimit(System
                .getProperty("erlide.process.heap.warn.limit", "10"));
        setKillProcessSizeLimit(System
                .getProperty("erlide.process.heap.kill.limit", "50"));
        setMaxParallelBuilds(System.getProperty("erlide.max.parallel.builds", "4"));
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

    public static boolean hasFeatureEnabled(final String feature) {
        return Boolean.parseBoolean(System.getProperty(feature));
    }

    public void setWarnProcessSizeLimit(final String text) {
        try {
            warnProcessSizeLimitMB = Integer.parseInt(text);
        } catch (final Exception e) {
            warnProcessSizeLimitMB = 10;
        }
        warnProcessSizeLimitMB = Math.max(warnProcessSizeLimitMB, MIN_WARN_LIMIT);
        if (warnProcessSizeLimitMB >= killProcessSizeLimitMB) {
            killProcessSizeLimitMB = warnProcessSizeLimitMB + 1;
        }
    }

    public void setKillProcessSizeLimit(final String text) {
        try {
            killProcessSizeLimitMB = Integer.parseInt(text);
        } catch (final Exception e) {
            killProcessSizeLimitMB = 30;
        }
        killProcessSizeLimitMB = Math.max(killProcessSizeLimitMB, MIN_KILL_LIMIT);
        if (warnProcessSizeLimitMB >= killProcessSizeLimitMB) {
            warnProcessSizeLimitMB = killProcessSizeLimitMB - 1;
        }
    }

    public int getKillProcessSizeLimitMB() {
        return killProcessSizeLimitMB;
    }

    public int getWarnProcessSizeLimitMB() {
        return warnProcessSizeLimitMB;
    }

    public String getHomeDir() {
        final String u = System.getProperty("user.home");
        if (isOnWindows()) {
            final String d = System.getenv("HOMEDRIVE");
            final String p = System.getenv("HOMEPATH");
            return d != null && p != null ? d + p : u;
        }
        return u;
    }

    private void setMaxParallelBuilds(final String text) {
        try {
            maxParallelBuilds = Integer.parseInt(text);
        } catch (final Exception e) {
            maxParallelBuilds = 4;
        }
    }

    public int getMaxParallelBuilds() {
        return maxParallelBuilds;
    }
}
