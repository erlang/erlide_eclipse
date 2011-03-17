package org.erlide.core.model.erlang.internal;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.ErlangCore;
import org.erlide.core.common.CommonUtils;
import org.erlide.core.model.erlang.ErlModelException;
import org.erlide.core.model.erlang.ErlModelStatusConstants;
import org.erlide.core.model.erlang.IErlElement;
import org.erlide.core.model.erlang.IErlFolder;
import org.erlide.core.model.erlang.IErlModelManager;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.IErlProject;
import org.erlide.core.model.erlang.IParent;

/**
 * Implementation of folder in erlang model
 * 
 * @author Jakob C
 * 
 */
public class ErlFolder extends Openable implements IErlFolder {
    private final IFolder folder;

    protected ErlFolder(final IFolder folder, final IParent parent) {
        super(parent, folder.getName());
        this.folder = folder;
    }

    @Override
    protected boolean buildStructure(final IProgressMonitor pm)
            throws ErlModelException {
        final IErlModelManager manager = ErlangCore.getModelManager();
        final IContainer c = (IContainer) getResource();
        try {
            // FIXME this is general stuff, should we put it in, say, model or
            // model manager?
            final IResource[] members = c.members();
            for (final IResource resource : members) {
                manager.create(resource);
            }
        } catch (final CoreException e) {
            throw new ErlModelException(e,
                    ErlModelStatusConstants.CORE_EXCEPTION);
        }
        return true;
    }

    @Override
    protected void closing(final Object info) throws ErlModelException {
        // FIXME general stuff, move to model or manager
        for (final IErlElement e : getChildren()) {
            if (e instanceof ErlElement) {
                final ErlElement ee = (ErlElement) e;
                ee.closing(info);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.erlide.core.model.erlang.IErlFolder#getModules()
     */
    public Collection<IErlModule> getModules() throws ErlModelException {
        final List<IErlModule> result = new ArrayList<IErlModule>();
        addModules(result);
        return result;
    }

    public Collection<IResource> getNonErlangResources()
            throws ErlModelException {
        return null;
    }

    public Kind getKind() {
        return Kind.FOLDER;
    }

    @Override
    public IResource getResource() {
        return getCorrespondingResource();
    }

    @Override
    public IResource getCorrespondingResource() {
        return folder;
    }

    public boolean isOnSourcePath() {
        final IErlProject project = getProject();
        return ErlFolder.isOnPaths(folder, project.getWorkspaceProject(),
                project.getSourceDirs());
    }

    public boolean isOnIncludePath() {
        final IErlProject project = getProject();
        return ErlFolder.isOnPaths(folder, project.getWorkspaceProject(),
                project.getIncludeDirs());
    }

    public boolean isSourcePathParent() {
        final IProject project = folder.getProject();
        /*
         * Get the project settings so that we can find the source nodes
         */
        final IErlProject erlProject = ErlangCore.getModel().getErlangProject(
                project);
        final Collection<IPath> sourcePaths = erlProject.getSourceDirs();
        final IPath path = folder.getFullPath();
        for (final IPath i : sourcePaths) {
            if (path.isPrefixOf(project.getFolder(i).getFullPath())) {
                return true;
            }
        }
        return false;
    }

    public static boolean isOnPaths(final IContainer con,
            final IContainer project, final Collection<IPath> paths) {
        final IPath path = con.getFullPath();
        for (final IPath i : paths) {
            if (i.toString().equals(".")) {
                if (project.getFullPath().equals(path)) {
                    return true;
                }
            } else if (project.getFolder(i).getFullPath().equals(path)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public void setChildren(final Collection<? extends IErlElement> c) {
        if (isOnIncludePath() || isOnSourcePath()) {
            ErlModel.getErlModelCache().removeProject(getProject());
        }
        super.setChildren(c);
    }

    @Override
    public void clearCaches() {
        if (isOnIncludePath() || isOnSourcePath()) {
            ErlModel.getErlModelCache().removeProject(getProject());
        }
        super.clearCaches();
    }

    public IErlModule findModule(final String moduleName,
            final String modulePath) throws ErlModelException {
        final Collection<IErlModule> modules = getModules();
        if (modulePath != null) {
            for (final IErlModule module : modules) {
                final String path = module.getFilePath();
                if (path != null && path.equals(modulePath)) {
                    return module;
                }
            }
        }
        boolean hasExtension;
        if (moduleName != null) {
            hasExtension = CommonUtils.hasExtension(moduleName);
            for (final IErlModule module : modules) {
                final String name = hasExtension ? module.getName() : module
                        .getModuleName();
                if (name.equals(moduleName)) {
                    return module;
                }
            }
        }
        return null;
    }
}
