package org.erlide.runtime.api;

public interface IRuntimeProcess {

    // TODO should take cmdline?
    // TODO should have a factory instead?

    void stop();

    boolean isRunning();

    // TODO boolean isCrashed();

    // TODO int exitCode();

}
