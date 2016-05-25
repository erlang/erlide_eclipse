package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.IErlangPreprocessor;

@SuppressWarnings("all")
public interface IErlangUndef extends IErlangPreprocessor {
  @NonNull
  public abstract String getName();
}
