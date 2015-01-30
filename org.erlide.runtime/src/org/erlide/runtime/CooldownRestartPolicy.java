package org.erlide.runtime;

public class CooldownRestartPolicy extends ServiceRestartPolicy {
    /**
     * If restarting sooner than this, it's probably an unrecoverable error.
     */
    public static final int RESTART_INTERVAL = 5000;

    private long last;
    private long interval = RESTART_INTERVAL;

    public CooldownRestartPolicy() {
        this(RESTART_INTERVAL);
    }

    public CooldownRestartPolicy(final long interval) {
        this.interval = interval;
        last = System.currentTimeMillis();
    }

    @Override
    public void notifyRestart() {
        last = System.currentTimeMillis();
    }

    @Override
    public boolean shouldRestart() {
        return System.currentTimeMillis() - last > interval;
    }

}
