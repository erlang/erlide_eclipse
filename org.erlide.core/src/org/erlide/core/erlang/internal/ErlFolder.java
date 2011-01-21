package org.erlide.core.erlang.internal;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlModelStatusConstants;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFolder;
import org.erlide.core.erlang.IErlModelManager;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IOpenable;
import org.erlide.core.erlang.IParent;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.core.erlang.util.PluginUtils;

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
                manager.create(resource, this);
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
    public static IErlModule getModule(final IParent parent, final String name,
            final boolean caseinsensitive) {
        try {
            if (parent instanceof IOpenable) {
                final IOpenable o = (IOpenable) parent;
                o.open(null);
            }
            final boolean hasExtension = ErlideUtil.hasExtension(name);
            for (final IErlElement e : parent.getChildren()) {
                if (e instanceof IErlModule) {
                    final IErlModule m = (IErlModule) e;
                    final String moduleName = hasExtension ? m.getName() : m
                            .getModuleName();
                    if (caseinsensitive) {
                        if (moduleName.equalsIgnoreCase(name)) {
                            return m;
                        }
                    } else {
                        if (moduleName.equals(name)) {
                            return m;
                        }
                    }
                } else if (e instanceof IParent) {
                    final IParent p = (IParent) e;
                    final IErlModule m = getModule(p, name, caseinsensitive);
                    if (m != null) {
                        return m;
                    }
                }
            }
        } catch (final ErlModelException e) {
            e.printStackTrace();
        }
        return null;
    }

    public IErlModule getModule(final String name) throws ErlModelException {
        return getModule(this, name, false);
    }

    public IErlModule getModuleExt(final String name) {
        return getModule(this, name, true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.erlide.core.erlang.IErlFolder#getModules()
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
        return PluginUtils.isOnSourcePath(folder);
    }

    public boolean isSourcePathParent() {
        return PluginUtils.isSourcePathParent(folder);
    }
}
