package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.IErlangElement;
import org.erlide.engine.model.news.IErlangForm;

@SuppressWarnings("all")
public interface IErlangUnit extends IErlangElement {
  public abstract String getFileExtension();
  
  @NonNull
  public abstract Iterable<IErlangForm> getForms();
}
