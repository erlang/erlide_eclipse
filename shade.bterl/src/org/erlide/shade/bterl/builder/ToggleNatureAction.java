package org.erlide.shade.bterl.builder;

import java.util.Iterator;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

public class ToggleNatureAction implements IObjectActionDelegate {

    private ISelection selection;

    @Override
    @SuppressWarnings({ "rawtypes" })
    public void run(final IAction action) {
        if (selection instanceof IStructuredSelection) {
            for (final Iterator it = ((IStructuredSelection) selection)
                    .iterator(); it.hasNext();) {
                final Object element = it.next();
                IProject project = null;
                if (element instanceof IProject) {
                    project = (IProject) element;
                } else if (element instanceof IAdaptable) {
                    project = (IProject) ((IAdaptable) element)
                            .getAdapter(IProject.class);
                }
                if (project != null) {
                    toggleNature(project);
                }
            }
        }
    }

    @Override
    public void selectionChanged(final IAction action,
            final ISelection theSelection) {
        selection = theSelection;
    }

    @Override
    public void setActivePart(final IAction action,
            final IWorkbenchPart targetPart) {
    }

    /**
     * Toggles sample nature on a project
     * 
     * @param project
     *            to have sample nature added or removed
     */
    private void toggleNature(final IProject project) {
        try {
            final IProjectDescription description = project.getDescription();
            final String[] natures = description.getNatureIds();

            for (int i = 0; i < natures.length; ++i) {
                if (ErlTestNature.NATURE_ID.equals(natures[i])) {
                    // Remove the nature
                    final String[] newNatures = new String[natures.length - 1];
                    System.arraycopy(natures, 0, newNatures, 0, i);
                    System.arraycopy(natures, i + 1, newNatures, i,
                            natures.length - i - 1);
                    description.setNatureIds(newNatures);
                    project.setDescription(description, null);
                    return;
                }
            }

            // Add the nature
            final String[] newNatures = new String[natures.length + 1];
            System.arraycopy(natures, 0, newNatures, 0, natures.length);
            newNatures[natures.length] = ErlTestNature.NATURE_ID;
            description.setNatureIds(newNatures);
            project.setDescription(description, null);
        } catch (final CoreException e) {
        }
    }

}
