package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.IErlangExpression;
import org.erlide.engine.model.news.IErlangForm;

@SuppressWarnings("all")
public interface IErlangError extends IErlangForm, IErlangExpression {
  @NonNull
  public abstract String getMessage();
}
