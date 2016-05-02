package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.IErlangModuleRef;

@SuppressWarnings("all")
public interface IErlangFunctionRef extends IErlangModuleRef {
  @NonNull
  public abstract String getName();
  
  public abstract int getArity();
}
