package org.erlide.backend.api;

public interface IPluginCodeLoader {

    public abstract void registerCodeBundle(ICodeBundle bundle);

    public abstract void unregisterCodeBundle(ICodeBundle bundle);

}
