package org.erlide.engine.model.news;

@SuppressWarnings("all")
public class ErlangElements {
    public static IErlangProject getProject(final IErlangLibrary library) {
        final IErlangElement _parent = library.getParent();
        return (IErlangProject) _parent;
    }
}
