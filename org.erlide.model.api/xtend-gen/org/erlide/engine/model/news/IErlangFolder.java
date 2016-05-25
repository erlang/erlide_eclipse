package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.IErlangElement;
import org.erlide.engine.model.news.IErlangUnit;

@SuppressWarnings("all")
public interface IErlangFolder extends IErlangElement {
  @NonNull
  public abstract Iterable<? extends IErlangUnit> getUnits();
  
  public abstract IErlangUnit getUnit(final String name);
}
