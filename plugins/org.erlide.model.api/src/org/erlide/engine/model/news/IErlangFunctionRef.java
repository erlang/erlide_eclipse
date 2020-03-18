package org.erlide.engine.model.news;

@SuppressWarnings("all")
public interface IErlangFunctionRef extends IErlangModuleRef {
    String getName();

    int getArity();
}
