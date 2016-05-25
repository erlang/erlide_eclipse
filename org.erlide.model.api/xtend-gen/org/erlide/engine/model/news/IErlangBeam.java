package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.ErlangBeamProperties;
import org.erlide.engine.model.news.IErlangUnit;

@SuppressWarnings("all")
public interface IErlangBeam extends IErlangUnit {
  @NonNull
  public abstract ErlangBeamProperties getProperties();
}
