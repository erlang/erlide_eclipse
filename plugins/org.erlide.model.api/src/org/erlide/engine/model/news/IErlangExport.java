package org.erlide.engine.model.news;

@SuppressWarnings("all")
public interface IErlangExport extends IErlangAttribute {
    Iterable<IErlangFunctionRef> getFunctions();
}
