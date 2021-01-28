package org.erlide.engine.model.news;

@SuppressWarnings("all")
public interface IErlangFolder extends IErlangElement {
    Iterable<? extends IErlangUnit> getUnits();

    IErlangUnit getUnit(final String name);
}
