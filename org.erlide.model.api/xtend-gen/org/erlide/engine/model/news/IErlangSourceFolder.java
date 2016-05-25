package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.IErlangFolder;
import org.erlide.engine.model.news.IErlangSource;

@SuppressWarnings("all")
public interface IErlangSourceFolder extends IErlangFolder {
  @NonNull
  public abstract Iterable<? extends IErlangFolder> getFolders();
  
  public abstract IErlangFolder getFolder(final String name);
  
  @NonNull
  public abstract Iterable<? extends IErlangSource> getSources();
  
  public abstract IErlangSource getSource(final String name);
}
