package org.erlide.backend.api;

public interface IPluginCodeLoader {

    void registerCodeBundle(ICodeBundle bundle);

    void unregisterCodeBundle(ICodeBundle bundle);

}
