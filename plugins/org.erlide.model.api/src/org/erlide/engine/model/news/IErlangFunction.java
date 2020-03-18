package org.erlide.engine.model.news;

@SuppressWarnings("all")
public interface IErlangFunction extends IErlangForm {
    String getName();

    int getArity();

    Iterable<IErlangFunctionClause> getClauses();

    IErlangComment getComment();
}
