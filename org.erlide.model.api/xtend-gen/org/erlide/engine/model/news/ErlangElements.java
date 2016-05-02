package org.erlide.engine.model.news;

import org.erlide.engine.model.news.IErlangElement;
import org.erlide.engine.model.news.IErlangLibrary;
import org.erlide.engine.model.news.IErlangProject;

@SuppressWarnings("all")
public class ErlangElements {
  public static IErlangProject getProject(final IErlangLibrary library) {
    IErlangElement _parent = library.getParent();
    return ((IErlangProject) _parent);
  }
}
