package org.erlide.runtime.service;

public class NeverRestartPolicy extends ServiceRestartPolicy {

    @Override
    public boolean shouldRestart() {
        return false;
    }

}
