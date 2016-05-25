package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.IErlangForm;

@SuppressWarnings("all")
public interface IErlangAttribute extends IErlangForm {
  @NonNull
  public abstract String getTag();
}
