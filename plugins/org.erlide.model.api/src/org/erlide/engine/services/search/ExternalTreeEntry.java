package org.erlide.engine.services.search;

import org.eclipse.core.runtime.IPath;

public class ExternalTreeEntry {
    private final IPath parentPath;
    private final IPath path;
    private final boolean isModule;

    public ExternalTreeEntry(final IPath parentPath, final IPath path,
            final boolean isModule) {
        super();
        this.parentPath = parentPath;
        this.path = path;
        this.isModule = isModule;
    }

    public IPath getParentPath() {
        return parentPath;
    }

    public IPath getPath() {
        return path;
    }

    public boolean isModule() {
        return isModule;
    }
}
