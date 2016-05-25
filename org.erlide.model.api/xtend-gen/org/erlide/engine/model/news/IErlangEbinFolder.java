package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.IErlangBeam;
import org.erlide.engine.model.news.IErlangFolder;

@SuppressWarnings("all")
public interface IErlangEbinFolder extends IErlangFolder {
  @NonNull
  public abstract Iterable<? extends IErlangBeam> getBeams();
  
  public abstract IErlangBeam getBeam(final String name);
}
