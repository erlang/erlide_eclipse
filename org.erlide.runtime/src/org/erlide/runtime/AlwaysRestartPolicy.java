package org.erlide.runtime;

public class AlwaysRestartPolicy extends ServiceRestartPolicy {

    @Override
    public boolean shouldRestart() {
        return true;
    }

}
