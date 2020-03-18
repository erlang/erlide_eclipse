package org.erlide.engine.model.news;

@SuppressWarnings("all")
public interface IErlangImport extends IErlangAttribute {
    Iterable<IErlangFunctionRef> getFunctionRefs();
}
