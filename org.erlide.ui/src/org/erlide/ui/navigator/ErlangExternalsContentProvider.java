package org.erlide.ui.navigator;

import java.util.Collection;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlElement.Kind;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IOpenable;
import org.erlide.core.erlang.IParent;
import org.erlide.core.erlang.util.ModelUtils;
import org.erlide.jinterface.util.ErlLogger;

public class ErlangExternalsContentProvider implements ITreeContentProvider {
    // ITreePathContentProvider

    ErlangFileContentProvider erlangFileContentProvider = new ErlangFileContentProvider();

    public ErlangExternalsContentProvider() {
        super();
        // TODO Auto-generated constructor stub
    }

    private static final Object[] NO_CHILDREN = new Object[0];

    public Object[] getElements(final Object inputElement) {
        return getChildren(inputElement);
    }

    public void dispose() {
        erlangFileContentProvider.dispose();
    }

    public void inputChanged(final Viewer viewer, final Object oldInput,
            final Object newInput) {
        // TODO Auto-generated method stub

    }

    public Object[] getChildren(Object parentElement) {
        try {
            if (parentElement instanceof IProject) {
                final IProject project = (IProject) parentElement;
                if (project.isOpen()) {
                    parentElement = ErlangCore.getModel().findProject(project);
                }
            }
            if (parentElement instanceof IErlModule) {
                return erlangFileContentProvider.getChildren(parentElement);
            }
            if (parentElement instanceof IParent) {
                if (parentElement instanceof IOpenable) {
                    final IOpenable openable = (IOpenable) parentElement;
                    openable.open(null); // FIXME should this really be
                                         // necessary?
                }
                final IParent parent = (IParent) parentElement;
                final Collection<IErlElement> children = parent
                        .getChildrenOfKind(Kind.EXTERNAL);
                return children.toArray();
            }
        } catch (final ErlModelException e) {
        }
        return NO_CHILDREN;
    }

    public Object getParent(final Object element) {
        if (element instanceof IErlProject) {
            final IErlProject project = (IErlProject) element;
            return project.getProject();
        }
        if (element instanceof IErlElement) {
            final IErlElement elt = (IErlElement) element;
            IParent parent = elt.getParent();
            final String filePath = elt.getFilePath();
            if (parent == ErlangCore.getModel() && filePath != null) {
                try {
                    // FIXME shouldn't this call be assigned to something!?
                    ModelUtils.findExternalModuleFromPath(filePath);
                } catch (final CoreException e) {
                }
                parent = elt.getParent();
            }
            if (parent instanceof IErlModule) {
                final IErlModule mod = (IErlModule) parent;
                try {
                    final IResource resource = mod.getCorrespondingResource();
                    if (resource != null) {
                        return resource;
                    }
                } catch (final ErlModelException e) {
                    ErlLogger.warn(e);
                }
            } else {
                return parent;
            }
        }
        return null;
    }

    public boolean hasChildren(Object element) {
        if (element instanceof IProject) {
            final IProject project = (IProject) element;
            if (project.isOpen()) {
                element = ErlangCore.getModel().findProject(project);
            }
        }
        if (element instanceof IErlModule) {
            return erlangFileContentProvider.hasChildren(element);
        }
        if (element instanceof IParent) {
            if (element instanceof IOpenable) {
                final IOpenable openable = (IOpenable) element;
                // FIXME should this really be necessary?
                try {
                    openable.open(null);
                } catch (final ErlModelException e) {
                }
            }
            final IParent parent = (IParent) element;
            return parent.hasChildrenOfKind(Kind.EXTERNAL)
                    || parent.hasChildrenOfKind(Kind.MODULE);
        }
        return false;
    }
    //
    // public Object[] getChildren(final TreePath parentPath) {
    // // TODO Auto-generated method stub
    // return null;
    // }
    //
    // public boolean hasChildren(final TreePath path) {
    // // TODO Auto-generated method stub
    // return false;
    // }
    //
    // public TreePath[] getParents(final Object element) {
    // // TODO Auto-generated method stub
    // return null;
    // }

}
