package org.erlide.cover.api;

/**
 * This interface's sole purpose is to get a singleton through Eclipse's
 * extension point mechanism.
 */

public interface ICoveragePerformerProxy {
    String ID = "org.erlide.cover.analysis";

    ICoveragePerformer getPerformer();

    ICoverBackend getBackend();
}
