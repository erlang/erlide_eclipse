package org.erlide.engine.model.news;

@SuppressWarnings("all")
public interface IErlangRecordDef extends IErlangAttribute {
    String getName();

    IErlangExpression getDefinition();
}
