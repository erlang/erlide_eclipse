package org.erlide.core.executor;

public interface ProgressCallback {

    void stdout(String line);

    void stderr(String line);

}
