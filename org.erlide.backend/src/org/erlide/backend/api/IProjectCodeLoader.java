package org.erlide.backend.api;

import org.erlide.engine.model.root.IErlProject;

public interface IProjectCodeLoader {

    void addProjectPath(IErlProject project);

    void removeProjectPath(IErlProject project);

}
