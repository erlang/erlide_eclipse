package org.erlide.engine.model.news;

@SuppressWarnings("all")
public interface IErlangSourceFolder extends IErlangFolder {
    Iterable<? extends IErlangFolder> getFolders();

    IErlangFolder getFolder(final String name);

    Iterable<? extends IErlangSource> getSources();

    IErlangSource getSource(final String name);
}
