package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.IErlangComment;
import org.erlide.engine.model.news.IErlangError;
import org.erlide.engine.model.news.IErlangUnit;
import org.erlide.engine.model.news.ISourceFile;

@SuppressWarnings("all")
public interface IErlangSource extends IErlangUnit, ISourceFile {
  public abstract IErlangComment getHeaderComment();
  
  @NonNull
  public abstract Iterable<IErlangError> getErrors();
}
