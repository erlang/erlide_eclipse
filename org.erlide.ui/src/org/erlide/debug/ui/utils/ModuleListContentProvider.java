package org.erlide.debug.ui.utils;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlModel;
import org.erlide.launch.ErlLaunchAttributes;
import org.erlide.ui.launch.DebugTreeItem;

public class ModuleListContentProvider implements
        IStructuredContentProvider {
    final boolean DISABLED = true;

    public ModuleListContentProvider() {
        super();
    }

    @Override
    public void inputChanged(final Viewer viewer, final Object oldInput,
            final Object newInput) {
        try {
            setRoot(new DebugTreeItem(null, null));
            if (DISABLED) {
                return;
            }
            if (newInput instanceof ILaunchConfiguration) {
                final ILaunchConfiguration input = (ILaunchConfiguration) newInput;
                final String projs = input.getAttribute(
                        ErlLaunchAttributes.PROJECTS, "").trim();
                if (projs.length() == 0) {
                    return;
                }
                final String[] projNames = projs.split(";");
                if (projNames == null) {
                    return;
                }
                final IErlModel model = ErlModelManager.getErlangModel();
                for (final String projName : projNames) {
                    final IErlElement prj = model.getChildNamed(projName);
                    getRoot().addAllErlangModules(prj);
                }
            }
        } catch (final CoreException e1) {
        }
    }

    @Override
    public void dispose() {
    }

    @Override
    public Object[] getElements(final Object inputElement) {
        return getChildren(getRoot());
    }

    @Override
    public Object[] getChildren(final Object parentElement) {
        final DebugTreeItem dti = (DebugTreeItem) parentElement;
        return dti.children.toArray();
    }

    @Override
    public Object getParent(final Object element) {
        final DebugTreeItem dti = (DebugTreeItem) element;
        return dti.getParent();
    }

    @Override
    public boolean hasChildren(final Object element) {
        return getChildren(element).length > 0;
    }

    public DebugTreeItem getRoot() {
        return root;
    }

    public void setRoot(final DebugTreeItem root) {
        this.root = root;
    }
}