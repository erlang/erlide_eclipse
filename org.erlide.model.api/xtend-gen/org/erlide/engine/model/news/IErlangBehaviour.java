package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.IErlangAttribute;

@SuppressWarnings("all")
public interface IErlangBehaviour extends IErlangAttribute {
  @NonNull
  public abstract String getName();
}
