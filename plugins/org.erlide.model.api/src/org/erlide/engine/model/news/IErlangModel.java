package org.erlide.engine.model.news;

@SuppressWarnings("all")
public interface IErlangModel extends IErlangElement {
    Iterable<IErlangProject> getProjects();

    IErlangProject getProject(final String name);
}
