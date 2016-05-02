package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.IErlangExpression;
import org.erlide.engine.model.news.IErlangPreprocessor;

@SuppressWarnings("all")
public interface IErlangDefine extends IErlangPreprocessor {
  @NonNull
  public abstract String getName();
  
  public abstract IErlangExpression getValue();
}
