package org.erlide.core.preferences;

import org.eclipse.core.runtime.IPath;

public class IncludeEntry extends PathEntry {

    public IncludeEntry(IPath path) {
        super(path);
    }

    @Override
    public boolean isRunTime() {
        return false;
    }

    @Override
    public boolean isCompileTime() {
        return true;
    }
}
