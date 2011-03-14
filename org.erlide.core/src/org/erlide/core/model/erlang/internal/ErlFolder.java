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

    /**
     * Find named module, search recursively. Should we use a visitor instead?
     * 
     * @param parent
     * @param name
     * @return
     */
    // public static IErlModule getModule(final IParent parent, final String
    // name,
    // final boolean caseinsensitive) {
    // try {
    // if (parent instanceof IOpenable) {
    // final IOpenable o = (IOpenable) parent;
    // o.open(null);
    // }
    // final boolean hasExtension = ErlideUtil.hasExtension(name);
    // for (final IErlElement e : parent.getChildren()) {
    // if (e instanceof IErlModule) {
    // final IErlModule m = (IErlModule) e;
    // final String moduleName = hasExtension ? m.getName() : m
    // .getModuleName();
    // if (caseinsensitive) {
    // if (moduleName.equalsIgnoreCase(name)) {
    // return m;
    // }
    // } else {
    // if (moduleName.equals(name)) {
    // return m;
    // }
    // }
    // } else if (e instanceof IParent) {
    // final IParent p = (IParent) e;
    // final IErlModule m = getModule(p, name, caseinsensitive);
    // if (m != null) {
    // return m;
    // }
    // }
    // }
    // } catch (final ErlModelException e) {
    // e.printStackTrace();
    // }
    // return null;
    // }
    //
    // public IErlModule getModule(final String name) throws ErlModelException {
    // return getModule(this, name, false);
    // }
    //
    // public IErlModule getModuleExt(final String name) {
    // return getModule(this, name, true);
    // }

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
        final IErlProject erlProject = getErlProject();
        return ErlFolder.isOnPaths(folder, erlProject.getProject(),
                erlProject.getSourceDirs());
    }

    public boolean isOnIncludePath() {
        final IErlProject erlProject = getErlProject();
        return ErlFolder.isOnPaths(folder, erlProject.getProject(),
                erlProject.getIncludeDirs());
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
            final IProject project, final Collection<IPath> paths) {
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
            ErlModel.getErlModelCache().removeForProject(getErlProject());
        }
        super.setChildren(c);
    }

    @Override
    public void clearCaches() {
        if (isOnIncludePath() || isOnSourcePath()) {
            ErlModel.getErlModelCache().removeForProject(getErlProject());
        }
        super.clearCaches();
    }

    public IErlModule findModule(final String includeName,
            final String includePath) throws ErlModelException {
        final Collection<IErlModule> modules = getModules();
        if (includePath != null) {
            for (final IErlModule module : modules) {
                final String path = module.getFilePath();
                if (path != null && path.equals(includePath)) {
                    return module;
                }
            }
        }
        boolean hasExtension;
        if (includeName != null) {
            hasExtension = CommonUtils.hasExtension(includeName);
            for (final IErlModule module : modules) {
                final String name = hasExtension ? module.getName() : module
                        .getModuleName();
                if (name.equals(includeName)) {
                    return module;
                }
            }
        }
        return null;
    }
}
