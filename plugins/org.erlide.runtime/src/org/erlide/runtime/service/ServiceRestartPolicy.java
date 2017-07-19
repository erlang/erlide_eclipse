package org.erlide.runtime.service;

public abstract class ServiceRestartPolicy {

    /**
     * Policy might want to keep track of when the latest restarts have
     * happened.
     */
    public void notifyRestart() {
    }

    abstract public boolean shouldRestart();

}
