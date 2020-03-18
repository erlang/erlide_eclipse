package org.erlide.engine.model.news;

@SuppressWarnings("all")
public interface IErlangTypeSpec extends IErlangAttribute {
    IErlangFunctionRef getFunction();

    IErlangExpression getSpec();
}
