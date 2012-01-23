package org.erlide.ui.navigator;

import java.util.Collection;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlElement.Kind;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.model.root.IOpenable;
import org.erlide.core.model.root.IParent;

public class ErlangExternalsContentProvider implements ITreeContentProvider {
    // ITreePathContentProvider

    ErlangFileContentProvider erlangFileContentProvider = new ErlangFileContentProvider();

    public ErlangExternalsContentProvider() {
        super();
    }

    private static final Object[] NO_CHILDREN = new Object[0];

    @Override
    public Object[] getElements(final Object inputElement) {
        return getChildren(inputElement);
    }

    @Override
    public void dispose() {
        erlangFileContentProvider.dispose();
    }

    @Override
    public void inputChanged(final Viewer viewer, final Object oldInput,
            final Object newInput) {
        // TODO Auto-generated method stub

    }

    @Override
    public Object[] getChildren(Object parentElement) {
        try {
            if (parentElement instanceof IProject) {
                final IProject project = (IProject) parentElement;
                if (project.isOpen()) {
                    parentElement = ErlModelManager.getErlangModel()
                            .findProject(project);
                }
            }
            if (parentElement instanceof IErlModule) {
                return erlangFileContentProvider.getChildren(parentElement);
            }
            if (parentElement instanceof IParent) {
                if (parentElement instanceof IOpenable) {
                    final IOpenable openable = (IOpenable) parentElement;
                    openable.open(null);
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

    @Override
    public Object getParent(final Object element) {
        if (element instanceof IErlProject) {
            final IErlProject project = (IErlProject) element;
            return project.getProject();
        }
        if (element instanceof IErlElement) {
            final IErlElement elt = (IErlElement) element;
            IParent parent = elt.getParent();
            final String filePath = elt.getFilePath();
            if (parent == ErlModelManager.getErlangModel() && filePath != null) {
                // try {
                // FIXME shouldn't this call be assigned to something!?
                // ModelUtils.findModule(null, null, filePath,
                // Scope.ALL_PROJECTS);
                // } catch (final CoreException e) {
                // }
                parent = elt.getParent();
            }
            if (parent instanceof IErlModule) {
                final IErlModule mod = (IErlModule) parent;
                final IResource resource = mod.getCorrespondingResource();
                if (resource != null) {
                    return resource;
                }
            } else {
                return parent;
            }
        }
        return null;
    }

    @Override
    public boolean hasChildren(Object element) {
        if (element instanceof IProject) {
            final IProject project = (IProject) element;
            if (project.isOpen()) {
                element = ErlModelManager.getErlangModel().findProject(project);
            }
        }
        if (element instanceof IErlModule) {
            return erlangFileContentProvider.hasChildren(element);
        }
        if (element instanceof IParent) {
            if (element instanceof IOpenable) {
                final IOpenable openable = (IOpenable) element;
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

}
