package org.erlide.engine.model.news;

@SuppressWarnings("all")
public interface IErlangDefine extends IErlangPreprocessor {
    String getName();

    IErlangExpression getValue();
}
