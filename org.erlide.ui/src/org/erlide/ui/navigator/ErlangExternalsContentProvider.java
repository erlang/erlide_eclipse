package org.erlide.ui.navigator;

import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlElement.Kind;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IOpenable;
import org.erlide.core.erlang.IParent;
import org.erlide.jinterface.util.ErlLogger;

public class ErlangExternalsContentProvider implements ITreeContentProvider {

    public ErlangExternalsContentProvider() {
        super();
        // TODO Auto-generated constructor stub
    }

    private static final Object[] NO_CHILDREN = new Object[0];

    public Object[] getElements(final Object inputElement) {
        return getChildren(inputElement);
    }

    public void dispose() {
        // TODO Auto-generated method stub

    }

    public void inputChanged(final Viewer viewer, final Object oldInput,
            final Object newInput) {
        // TODO Auto-generated method stub

    }

    public Object[] getChildren(Object parentElement) {
        try {
            if (parentElement instanceof IProject) {
                final IProject project = (IProject) parentElement;
                parentElement = ErlangCore.getModel().findProject(project);
            }
            if (parentElement instanceof IParent) {
                if (parentElement instanceof IOpenable) {
                    final IOpenable openable = (IOpenable) parentElement;
                    openable.open(null); // FIXME should this really be
                                         // necessary?
                }
                final IParent parent = (IParent) parentElement;
                final List<IErlElement> children = parent
                        .getChildrenOfKind(Kind.EXTERNAL);
                return children.toArray();
            }
        } catch (final ErlModelException e) {
        }
        return NO_CHILDREN;
    }

    public Object getParent(final Object element) {
        if (element instanceof IErlElement) {
            final IErlElement elt = (IErlElement) element;
            final IErlElement parent = elt.getParent();
            if (parent instanceof IErlModule) {
                final IErlModule mod = (IErlModule) parent;
                try {
                    return mod.getCorrespondingResource();
                } catch (final ErlModelException e) {
                    ErlLogger.warn(e);
                }
            }
        }
        return null;
    }

    public boolean hasChildren(Object element) {
        if (element instanceof IProject) {
            final IProject project = (IProject) element;
            element = ErlangCore.getModel().findProject(project);
        }
        if (element instanceof IParent) {
            if (element instanceof IOpenable) {
                final IOpenable openable = (IOpenable) element;
                try {
                    openable.open(null);// FIXME should this really be
                                        // necessary?
                } catch (final ErlModelException e) {
                }
            }
            final IParent parent = (IParent) element;
            return parent.hasChildrenOfKind(Kind.EXTERNAL)
                    || parent.hasChildrenOfKind(Kind.MODULE);
        }
        return false;
    }

}
