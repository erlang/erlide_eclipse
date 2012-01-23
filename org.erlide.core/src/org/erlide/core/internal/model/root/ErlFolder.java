package org.erlide.core.internal.model.root;

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
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.ModuleKind;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelStatusConstants;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlFolder;
import org.erlide.core.model.root.IErlModel;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.model.root.IParent;
import org.erlide.utils.SystemUtils;

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
        final IErlModel model = getModel();
        final IContainer c = (IContainer) getResource();
        try {
            // FIXME this is general stuff, should we put it in model or
            // model manager?
            final IResource[] members = c.members();
            for (final IResource resource : members) {
                model.create(resource);
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

    @Override
    public Collection<IErlModule> getModules() throws ErlModelException {
        final List<IErlModule> result = new ArrayList<IErlModule>();
        addModules(result);
        return result;
    }

    @Override
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

    @Override
    public boolean isOnSourcePath() {
        final IErlProject project = getProject();
        return ErlFolder.isOnPaths(folder, project.getWorkspaceProject(),
                project.getSourceDirs());
    }

    @Override
    public boolean isOnIncludePath() {
        final IErlProject project = getProject();
        return ErlFolder.isOnPaths(folder, project.getWorkspaceProject(),
                project.getIncludeDirs());
    }

    @Override
    public boolean isSourcePathParent() {
        final IProject project = folder.getProject();
        final IErlProject erlProject = getProject();
        final Collection<IPath> sourcePaths = erlProject.getSourceDirs();
        final IPath path = folder.getFullPath();
        for (final IPath i : sourcePaths) {
            if (path.isPrefixOf(project.getFolder(i).getFullPath())) {
                return true;
            }
        }
        return false;
    }

    public static boolean isOnPaths(final IContainer container,
            final IContainer project, final Collection<IPath> paths) {
        final IPath containerPath = container.getFullPath();
        for (final IPath path : paths) {
            if (path.toString().equals(".")) {
                if (project.getFullPath().equals(containerPath)) {
                    return true;
                }
            } else if (project.getFolder(path).getFullPath()
                    .equals(containerPath)) {
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

    private IErlModule findModuleOrInclude(final String name,
            final String path, final boolean isInclude)
            throws ErlModelException {
        final Collection<IErlModule> modules = getModules();
        if (path != null) {
            for (final IErlModule module : modules) {
                final String filePath = module.getFilePath();
                if (filePath != null && filePath.equals(path)) {
                    return module;
                }
            }
        }
        boolean hasExtension;
        if (name != null) {
            hasExtension = SystemUtils.hasExtension(name);
            for (final IErlModule module : modules) {
                final String name2 = module.getName();
                final String moduleName = hasExtension ? name2 : module
                        .getModuleName();
                if (name.equals(moduleName)) {
                    if (hasExtension
                            || isInclude == ModuleKind.hasHrlExtension(name2)) {
                        return module;
                    }
                }
            }
        }
        return null;
    }

    @Override
    public IErlModule findModule(final String moduleName,
            final String modulePath) throws ErlModelException {
        return findModuleOrInclude(moduleName, modulePath, false);
    }

    @Override
    public IErlModule findInclude(final String includeName,
            final String includePath) throws ErlModelException {
        return findModuleOrInclude(includeName, includePath, true);
    }

    private void addModules(final List<IErlModule> modules)
            throws ErlModelException {
        for (final IErlElement e : getChildren()) {
            if (e instanceof IErlModule) {
                modules.add((IErlModule) e);
            } else if (e instanceof ErlFolder) {
                final ErlFolder f = (ErlFolder) e;
                f.addModules(modules);
            }
        }
    }
}
