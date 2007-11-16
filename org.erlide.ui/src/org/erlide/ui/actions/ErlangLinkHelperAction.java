package org.erlide.ui.actions;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.navigator.ILinkHelper;
import org.eclipse.ui.part.FileEditorInput;

public class ErlangLinkHelperAction implements ILinkHelper {

	public void activateEditor(IWorkbenchPage page,
			IStructuredSelection selection) {
		if (selection == null || selection.isEmpty()) {
			return;
		}

		Object firstElement = selection.getFirstElement();

		// if it is an erlang element, let's first get the actual object for
		// finding the editor
		// TODO

		// and now, if it is really a file...
		if (firstElement instanceof IFile) {
			IEditorInput fileInput = new FileEditorInput((IFile) firstElement);
			IEditorPart editor = null;
			if ((editor = page.findEditor(fileInput)) != null) {
				page.bringToTop(editor);
			}
		}

	}

	public IStructuredSelection findSelection(IEditorInput anInput) {
		if (anInput instanceof IFileEditorInput) {
			return new StructuredSelection(((IFileEditorInput) anInput)
					.getFile());
		}
		return StructuredSelection.EMPTY;
	}

}
