package org.erlide.ui.navigator.actions;

import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.AbstractTreeViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.actions.OpenResourceAction;
import org.eclipse.ui.navigator.ICommonActionExtensionSite;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlExternal;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.editors.util.EditorUtility;

import com.google.common.collect.Sets;

public class OpenErlangAction extends Action {

    private IErlElement selectedElement;
    private final ISelectionProvider provider;
    private final Set<IProject> selectedClosedProjects;
    private final OpenResourceAction openResourceAction;
    private final ICommonActionExtensionSite site;

    /**
     * Construct the OpenPropertyAction with the given page.
     * 
     * @param p
     *            The page to use as context to open the editor.
     * @param selectionProvider
     *            The selection provider
     */
    public OpenErlangAction(final ICommonActionExtensionSite site,
            final ISelectionProvider selectionProvider) {
        this.site = site;
        setText(Messages.getString("OpenErlangAction.0")); //$NON-NLS-1$
        provider = selectionProvider;
        selectedElement = null;
        selectedClosedProjects = Sets.newHashSet();
        final IShellProvider shellProvider = (IShellProvider) site
                .getViewSite().getAdapter(IShellProvider.class);
        openResourceAction = new OpenResourceAction(shellProvider);
    }

    @Override
    public boolean isEnabled() {
        selectedElement = null;
        selectedClosedProjects.clear();
        final ISelection selection = provider.getSelection();
        if (!selection.isEmpty()) {
            final IStructuredSelection sSelection = (IStructuredSelection) selection;
            if (sSelection.size() == 1
                    && sSelection.getFirstElement() instanceof IErlElement) {
                selectedElement = (IErlElement) sSelection.getFirstElement();
                return true;
            } else {
                for (final Object element : sSelection.toList()) {
                    if (element instanceof IProject) {
                        final IProject project = (IProject) element;
                        if (!project.isOpen()) {
                            selectedClosedProjects.add(project);
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }

    @Override
    public void run() {

        if (isEnabled()) {
            try {
                if (selectedElement != null) {
                    if (selectedElement instanceof IErlExternal) {
                        final StructuredViewer structuredViewer = site
                                .getStructuredViewer();
                        if (structuredViewer instanceof AbstractTreeViewer) {
                            final AbstractTreeViewer treeViewer = (AbstractTreeViewer) structuredViewer;
                            final boolean expanded = treeViewer
                                    .getExpandedState(selectedElement);
                            treeViewer.setExpandedState(selectedElement,
                                    !expanded);
                        }
                    } else {
                        final IEditorPart part = EditorUtility.openInEditor(
                                selectedElement, true);
                        EditorUtility.revealInEditor(part, selectedElement);
                    }
                } else if (!selectedClosedProjects.isEmpty()) {
                    openResourceAction
                            .selectionChanged((IStructuredSelection) provider
                                    .getSelection());
                    openResourceAction.run();
                }
            } catch (final PartInitException e) {
                ErlLogger.warn(e);
            }
        }
    }
}
