package org.erlide.runtime.service;

public class AlwaysRestartPolicy extends ServiceRestartPolicy {

    @Override
    public boolean shouldRestart() {
        return true;
    }

}
