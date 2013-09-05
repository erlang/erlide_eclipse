package org.erlide.backend.api;

import org.erlide.engine.model.root.IErlProject;

public interface IProjectCodeLoader {

    public abstract void addProjectPath(IErlProject project);

    public abstract void removeProjectPath(IErlProject project);

}
