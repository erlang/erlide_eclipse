package org.erlide.ui.actions;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.navigator.ILinkHelper;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.IErlElement;
import org.erlide.ui.editors.util.EditorUtility;
import org.erlide.ui.util.ErlModelUtils;

public class ErlangLinkHelperAction implements ILinkHelper {

    @Override
    public void activateEditor(final IWorkbenchPage page,
            final IStructuredSelection selection) {

        final Object element = selection.getFirstElement();
        final IEditorPart part = EditorUtility.isOpenInEditor(element);
        if (part != null) {
            page.bringToTop(part);
            if (element instanceof IErlElement) {
                EditorUtility.revealInEditor(part, (IErlElement) element);
            }
        }

        // if (selection == null || selection.isEmpty()) {
        // return;
        // }
        //
        // final Object firstElement = selection.getFirstElement();
        //
        // if (firstElement instanceof IErlElement) {
        // final IErlElement e = (IErlElement) firstElement;
        //
        // }
        // // if it is an erlang element, let's first get the actual object for
        // // finding the editor
        //
        // // and now, if it is really a file...
        // if (firstElement instanceof IFile) {
        // final IEditorInput fileInput = new FileEditorInput(
        // (IFile) firstElement);
        // IEditorPart editor = null;
        // if ((editor = page.findEditor(fileInput)) != null) {
        // page.bringToTop(editor);
        // }
        // }

    }

    @Override
    public IStructuredSelection findSelection(final IEditorInput input) {
        try {
            final IErlModule module = ErlModelUtils.getModule(input);
            if (module != null) {
                final IResource resource = module.getCorrespondingResource();
                if (resource != null) {
                    return new StructuredSelection(resource);
                } else {
                    return new StructuredSelection(module);
                }
            }
        } catch (final CoreException e) {
        }
        return StructuredSelection.EMPTY;
    }

}
