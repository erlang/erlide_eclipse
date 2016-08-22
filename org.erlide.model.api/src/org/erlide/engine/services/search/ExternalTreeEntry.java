package org.erlide.engine.services.search;

public class ExternalTreeEntry {
    private final String parentPath;
    private final String path;
    // private final String name;
    private final boolean isModule;

    public ExternalTreeEntry(final String parentPath, final String path,
            // final String name,
            final boolean isModule) {
        super();
        this.parentPath = parentPath;
        this.path = path;
        // this.name = name;
        this.isModule = isModule;
    }

    public String getParentPath() {
        return parentPath;
    }

    public String getPath() {
        return path;
    }

    public boolean isModule() {
        return isModule;
    }
}