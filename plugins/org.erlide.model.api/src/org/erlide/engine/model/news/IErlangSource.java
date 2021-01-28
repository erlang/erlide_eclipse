package org.erlide.engine.model.news;

@SuppressWarnings("all")
public interface IErlangSource extends IErlangUnit, ISourceFile {
    IErlangComment getHeaderComment();

    Iterable<IErlangError> getErrors();
}
