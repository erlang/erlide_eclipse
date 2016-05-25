package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.IErlangApplication;
import org.erlide.engine.model.news.IErlangElement;

@SuppressWarnings("all")
public interface IErlangLibrary extends IErlangElement {
  @NonNull
  public abstract Iterable<IErlangApplication> getApplications();
}
