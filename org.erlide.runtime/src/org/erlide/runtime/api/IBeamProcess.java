package org.erlide.runtime.api;

public interface IBeamProcess {

    // TODO should take cmdline?
    // TODO should have a factory instead?
    Process start();

    void stop();

    void restart();

    boolean isStopped();

    // TODO boolean isCrashed();

    // TODO int exitCode();

}
