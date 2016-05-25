package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;

@SuppressWarnings("all")
public interface IErlangElement {
  public abstract IErlangElement getParent();
  
  @NonNull
  public abstract Iterable<IErlangElement> getChildren();
  
  public abstract IErlangElement getChild(final String id);
}
