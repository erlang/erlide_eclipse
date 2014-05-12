package org.erlide.ui.navigator;

import java.util.Collection;
import java.util.concurrent.TimeUnit;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.IOpenable;
import org.erlide.engine.model.IParent;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlExternalRoot;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.util.ErlLogger;

import com.google.common.base.Stopwatch;

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
    }

    @Override
    public Object[] getChildren(final Object parentElement0) {
        Object parentElement = parentElement0;
        try {
            if (parentElement instanceof IProject) {
                final IProject project = (IProject) parentElement;
                if (project.isOpen()) {
                    parentElement = ErlangEngine.getInstance().getModel()
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
                final Collection<IErlElement> children = parent.getChildrenOfKind(
                        ErlElementKind.EXTERNAL_ROOT, ErlElementKind.EXTERNAL_APP,
                        ErlElementKind.EXTERNAL_FOLDER);
                return children.toArray();
            }
        } catch (final ErlModelException e) {
        }
        return NO_CHILDREN;
    }

    @Override
    public Object getParent(final Object element) {
        if (element instanceof IErlElement) {
            final IErlElement elt = (IErlElement) element;
            IParent parent = elt.getParent();
            final String filePath = elt.getFilePath();
            if (parent == ErlangEngine.getInstance().getModel() && filePath != null) {
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
    public boolean hasChildren(final Object element0) {
        Object element = element0;
        if (element instanceof IProject) {
            final IProject project = (IProject) element;
            if (project.isOpen()) {
                element = ErlangEngine.getInstance().getModel().findProject(project);
            }
        }
        if (element instanceof IErlModule) {
            return erlangFileContentProvider.hasChildren(element);
        }
        if (element instanceof IParent) {
            if (element instanceof IErlExternalRoot || element instanceof IErlProject
                    || element instanceof IErlModel) {
                // we know these have children
                return true;
            }
            final Stopwatch clock = Stopwatch.createStarted();
            if (element instanceof IOpenable) {
                final IOpenable openable = (IOpenable) element;
                try {
                    openable.open(null);
                } catch (final ErlModelException e) {
                }
            }
            final IParent parent = (IParent) element;
            final boolean result = parent.hasChildrenOfKind(ErlElementKind.EXTERNAL_ROOT,
                    ErlElementKind.EXTERNAL_APP, ErlElementKind.EXTERNAL_FOLDER,
                    ErlElementKind.MODULE);
            if (clock.elapsed(TimeUnit.MILLISECONDS) > 100) {
                ErlLogger.debug("TIME open " + element.getClass() + " " + element + "  "
                        + clock.elapsed(TimeUnit.MILLISECONDS) + " ms");
            }
            return result;
        }
        return false;
    }

}
