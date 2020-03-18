package org.erlide.engine.model.news;

@SuppressWarnings("all")
public interface IErlangLibrary extends IErlangElement {
    Iterable<IErlangApplication> getApplications();
}
