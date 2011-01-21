package org.erlide.ui.navigator.actions;

import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.actions.OpenResourceAction;
import org.erlide.core.erlang.IErlElement;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.ui.editors.util.EditorUtility;

import com.google.common.collect.Sets;

public class OpenErlangAction extends Action {

    private IErlElement selectedElement;
    private final ISelectionProvider provider;
    private final Set<IProject> selectedClosedProjects;
    private final OpenResourceAction openResourceAction;

    /**
     * Construct the OpenPropertyAction with the given page.
     * 
     * @param p
     *            The page to use as context to open the editor.
     * @param selectionProvider
     *            The selection provider
     */
    public OpenErlangAction(final IWorkbenchSite site,
            final ISelectionProvider selectionProvider) {
        setText(Messages.getString("OpenErlangAction.0")); //$NON-NLS-1$
        provider = selectionProvider;
        selectedElement = null;
        selectedClosedProjects = Sets.newHashSet();
        openResourceAction = new OpenResourceAction(site);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#isEnabled()
     */
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

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {

        if (isEnabled()) {
            try {
                if (selectedElement != null) {
                    final IEditorPart part = EditorUtility.openInEditor(
                            selectedElement, true);
                    EditorUtility.revealInEditor(part, selectedElement);
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
