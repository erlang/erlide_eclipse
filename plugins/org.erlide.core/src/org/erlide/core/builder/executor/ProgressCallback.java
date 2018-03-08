package org.erlide.core.builder.executor;

public interface ProgressCallback {

    void stdout(String line);

    void stderr(String line);

}
