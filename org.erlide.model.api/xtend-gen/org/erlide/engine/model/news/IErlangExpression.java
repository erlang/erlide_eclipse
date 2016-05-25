package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.IErlangElement;
import org.erlide.engine.model.news.ISourceConstruct;

@SuppressWarnings("all")
public interface IErlangExpression extends IErlangElement, ISourceConstruct {
  @NonNull
  public abstract String getContent();
}
