package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;

@SuppressWarnings("all")
public interface IErlangModuleRef {
  @NonNull
  public abstract String getModule();
}
