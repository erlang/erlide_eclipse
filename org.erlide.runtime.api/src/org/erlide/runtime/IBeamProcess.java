package org.erlide.runtime;

public interface IBeamProcess {

    // TODO should take cmdline?
    // TODO should have a factory instead?
    void start();

    void stop();

    void restart();

    boolean isStopped();

    // TODO boolean isCrashed();

    // TODO int exitCode();

}
