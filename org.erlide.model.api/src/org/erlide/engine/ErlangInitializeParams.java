package org.erlide.engine;

import io.typefox.lsapi.InitializeParams;

public interface ErlangInitializeParams extends InitializeParams {

    String getStateDir();

}
