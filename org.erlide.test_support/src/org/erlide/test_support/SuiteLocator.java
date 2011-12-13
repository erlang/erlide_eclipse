package org.erlide.test_support;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;

public class SuiteLocator {

    private final IProject project;

    public SuiteLocator(final IProject project) {
        this.project = project;
    }

    public List<IResource> findSuites() throws CoreException {
        final ResVisitor visitor = new ResVisitor();
        project.accept(visitor);
        return visitor.result;
    }

    protected class ResVisitor implements IResourceVisitor {

        public List<IResource> result = new ArrayList<IResource>();

        @Override
        public boolean visit(final IResource arg) throws CoreException {
            if (arg instanceof IFile && arg.getName().endsWith("_SUITE.erl")) {
                result.add(arg);
            }
            return true;
        }

    }

}
