package org.erlide.test_support.builder;

import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.jinterface.ErlLogger;

import com.google.common.collect.Lists;

public class TestProjectProperties {
    List<String> sources = Lists.newArrayList();

    private List<String> findTestDirs(final IProject prj) {
        final List<String> result = Lists.newArrayList();
        try {
            prj.accept(new IResourceVisitor() {
                @Override
                public boolean visit(final IResource resource)
                        throws CoreException {
                    if (resource.getName().matches(".*_SUITE.erl")) {
                        final IContainer dir = resource.getParent();
                        final IPath pdir = dir.getProjectRelativePath();
                        final String sdir = pdir.toString();
                        if (!sdir.contains("/garbage/")
                                && !result.contains(sdir)) {
                            result.add(sdir);
                        }
                    }
                    return true;
                }
            });
        } catch (final CoreException e) {
            ErlLogger.debug(e);
        }
        return result;
    }

    public boolean isInPath(final IResource resource, final IProject project) {
        final IPath projectPath = project.getFullPath();
        final IPath exceptLastSegment = resource.getFullPath()
                .removeLastSegments(1);
        for (final String element : sources) {
            final IPath sp = projectPath.append(new Path(element));
            if (sp.equals(exceptLastSegment)) {
                return true;
            }
        }

        return false;
    }
}
