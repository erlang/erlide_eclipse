package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.IErlangComment;
import org.erlide.engine.model.news.IErlangForm;
import org.erlide.engine.model.news.IErlangFunctionClause;

@SuppressWarnings("all")
public interface IErlangFunction extends IErlangForm {
  @NonNull
  public abstract String getName();
  
  public abstract int getArity();
  
  @NonNull
  public abstract Iterable<IErlangFunctionClause> getClauses();
  
  public abstract IErlangComment getComment();
}
