package org.erlide.test_support;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;

public class ProjectImporter {
    private final IProject project;

    public ProjectImporter(final IProject prj) {
        project = prj;
    }

    public void doImport() throws CoreException {
        for (final IProject prj : project.getReferencedProjects()) {
            importFromBase(prj);
        }
    }

    public void importFromBase(final IProject ref) {
        // locate dirs with suites and create linked resources for them

    }
}
