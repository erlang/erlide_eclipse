package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.IErlangAttribute;
import org.erlide.engine.model.news.IErlangFunctionRef;

@SuppressWarnings("all")
public interface IErlangExport extends IErlangAttribute {
  @NonNull
  public abstract Iterable<IErlangFunctionRef> getFunctions();
}
