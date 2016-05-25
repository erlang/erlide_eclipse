package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.ErlangApplicationProperties;
import org.erlide.engine.model.news.IErlangEbinFolder;
import org.erlide.engine.model.news.IErlangElement;
import org.erlide.engine.model.news.IErlangLibrary;
import org.erlide.engine.model.news.IErlangSourceFolder;

@SuppressWarnings("all")
public interface IErlangApplication extends IErlangElement {
  @NonNull
  public abstract ErlangApplicationProperties getProperties();
  
  @NonNull
  public abstract Iterable<IErlangSourceFolder> getSourceFolders();
  
  /**
   * @param path relative to application container
   */
  public abstract IErlangSourceFolder getSourceFolder(final String path);
  
  @NonNull
  public abstract Iterable<IErlangSourceFolder> getIncludeFolders();
  
  /**
   * @param path relative to application container
   */
  public abstract IErlangSourceFolder getIncludeFolder(final String path);
  
  @NonNull
  public abstract IErlangEbinFolder getBinaryFolder();
  
  @NonNull
  public abstract Iterable<IErlangSourceFolder> getTestFolders();
  
  /**
   * @param path relative to application container
   */
  public abstract IErlangSourceFolder getTestFolder(final String path);
  
  @NonNull
  public abstract IErlangLibrary getDependencies();
}
