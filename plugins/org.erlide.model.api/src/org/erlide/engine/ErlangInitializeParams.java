package org.erlide.engine;

import org.eclipse.lsp4j.InitializeParams;

public abstract class ErlangInitializeParams extends InitializeParams {

    public abstract String getStateDir();

}
