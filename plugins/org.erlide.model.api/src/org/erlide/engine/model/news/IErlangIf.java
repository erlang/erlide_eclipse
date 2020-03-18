package org.erlide.engine.model.news;

@SuppressWarnings("all")
public interface IErlangIf extends IErlangPreprocessor {
    String getCondition();

    Iterable<IErlangForm> getIfForms();

    Iterable<IErlangForm> getElseForms();
}
