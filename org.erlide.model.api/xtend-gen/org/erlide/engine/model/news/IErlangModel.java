package org.erlide.engine.model.news;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.news.IErlangElement;
import org.erlide.engine.model.news.IErlangProject;

@SuppressWarnings("all")
public interface IErlangModel extends IErlangElement {
  @NonNull
  public abstract Iterable<IErlangProject> getProjects();
  
  public abstract IErlangProject getProject(final String name);
}
