package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.IErlangElement;
import org.erlide.engine.model.news.IErlangExpression;
import org.erlide.engine.model.news.IErlangGuard;
import org.erlide.engine.model.news.ISourceConstruct;

@SuppressWarnings("all")
public interface IErlangFunctionClause extends IErlangElement, ISourceConstruct {
  @NonNull
  public abstract Iterable<IErlangExpression> getFormalParameters();
  
  public abstract IErlangGuard getGuard();
  
  public abstract IErlangExpression getBody();
}
