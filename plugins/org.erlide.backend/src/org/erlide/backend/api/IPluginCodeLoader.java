package org.erlide.backend.api;

import org.erlide.backend.api.ICodeBundle.CodeContext;

public interface IPluginCodeLoader {

    void registerCodeBundle(final CodeContext context, ICodeBundle bundle);

    void unregisterCodeBundle(final CodeContext context, ICodeBundle bundle);

}
