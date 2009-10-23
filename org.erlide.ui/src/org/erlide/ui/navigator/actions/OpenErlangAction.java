package org.erlide.ui.navigator.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlElement;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.ui.editors.util.EditorUtility;

public class OpenErlangAction extends Action {

	private IErlElement element;
	private final ISelectionProvider provider;

	/**
	 * Construct the OpenPropertyAction with the given page.
	 * 
	 * @param p
	 *            The page to use as context to open the editor.
	 * @param selectionProvider
	 *            The selection provider
	 */
	public OpenErlangAction(final IWorkbenchPage p,
			final ISelectionProvider selectionProvider) {
		setText(Messages.getString("OpenErlangAction.0")); //$NON-NLS-1$
		provider = selectionProvider;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.action.Action#isEnabled()
	 */
	@Override
	public boolean isEnabled() {
		final ISelection selection = provider.getSelection();
		if (!selection.isEmpty()) {
			final IStructuredSelection sSelection = (IStructuredSelection) selection;
			if (sSelection.size() == 1
					&& sSelection.getFirstElement() instanceof IErlElement) {
				element = ((IErlElement) sSelection.getFirstElement());
				return true;
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
				final IEditorPart part = EditorUtility.openInEditor(element,
						true);
				EditorUtility.revealInEditor(part, element);
			} catch (final PartInitException e) {
				ErlLogger.warn(e);
			} catch (final ErlModelException e) {
				ErlLogger.warn(e);
			}
		}
	}
}
