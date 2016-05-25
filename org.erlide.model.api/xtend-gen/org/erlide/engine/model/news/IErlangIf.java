package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.IErlangForm;
import org.erlide.engine.model.news.IErlangPreprocessor;

@SuppressWarnings("all")
public interface IErlangIf extends IErlangPreprocessor {
  @NonNull
  public abstract String getCondition();
  
  @NonNull
  public abstract Iterable<IErlangForm> getIfForms();
  
  @NonNull
  public abstract Iterable<IErlangForm> getElseForms();
}
