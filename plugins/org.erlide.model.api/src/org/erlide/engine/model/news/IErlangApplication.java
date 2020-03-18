package org.erlide.engine.model.news;

@SuppressWarnings("all")
public interface IErlangApplication extends IErlangElement {
    ErlangApplicationProperties getProperties();

    Iterable<IErlangSourceFolder> getSourceFolders();

    /**
     * @param path
     *            relative to application container
     */
    IErlangSourceFolder getSourceFolder(final String path);

    Iterable<IErlangSourceFolder> getIncludeFolders();

    /**
     * @param path
     *            relative to application container
     */
    IErlangSourceFolder getIncludeFolder(final String path);

    IErlangEbinFolder getBinaryFolder();

    Iterable<IErlangSourceFolder> getTestFolders();

    /**
     * @param path
     *            relative to application container
     */
    IErlangSourceFolder getTestFolder(final String path);

    IErlangLibrary getDependencies();
}
