package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.IErlangPreprocessor;

@SuppressWarnings("all")
public interface IErlangInclude extends IErlangPreprocessor {
  @NonNull
  public abstract String getFileName();
}
