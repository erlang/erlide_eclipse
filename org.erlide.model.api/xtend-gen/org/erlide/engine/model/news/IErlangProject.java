package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.IErlangApplication;
import org.erlide.engine.model.news.IErlangLibrary;

@SuppressWarnings("all")
public interface IErlangProject extends IErlangApplication {
  public final static String NATURE_ID = "org.erlide.core.erlnature";
  
  @NonNull
  public abstract String getOtpVersion();
  
  @NonNull
  public abstract IErlangLibrary getOtpLibrary();
}
