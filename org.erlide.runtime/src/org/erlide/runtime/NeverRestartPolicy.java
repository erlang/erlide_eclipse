package org.erlide.runtime;

public class NeverRestartPolicy extends ServiceRestartPolicy {

    @Override
    public boolean shouldRestart() {
        return false;
    }

}
