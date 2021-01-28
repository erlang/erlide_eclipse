package org.erlide.engine.model.news;

@SuppressWarnings("all")
public interface IErlangTypeDef extends IErlangAttribute {
    String getName();

    IErlangExpression getDefinition();
}
