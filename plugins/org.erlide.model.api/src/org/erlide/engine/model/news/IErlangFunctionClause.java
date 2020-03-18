package org.erlide.engine.model.news;

@SuppressWarnings("all")
public interface IErlangFunctionClause extends IErlangElement, ISourceConstruct {
    Iterable<IErlangExpression> getFormalParameters();

    IErlangGuard getGuard();

    IErlangExpression getBody();
}
