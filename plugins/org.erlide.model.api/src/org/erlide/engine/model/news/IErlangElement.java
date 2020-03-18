package org.erlide.engine.model.news;

@SuppressWarnings("all")
public interface IErlangElement {
    IErlangElement getParent();

    Iterable<IErlangElement> getChildren();

    IErlangElement getChild(final String id);
}
