package org.erlide.wrangler.refactoring.util;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.internal.Workbench;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModule;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.selection.internal.ErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.internal.ErlModuleSelection;
import org.erlide.wrangler.refactoring.selection.internal.ErlTextMemberSelection;

@SuppressWarnings("restriction")
public class GlobalParameters {
	// TODO:: handle null exceptions
	static IEditorPart editor = null;

	static IErlSelection wranglerSelection = null;;

	public static int getTabWidth() {
		return 1;
	}

	public static void setEditor(IEditorPart _editor) {
		editor = _editor;
		wranglerSelection = new ErlTextMemberSelection((ITextEditor) editor);
	}

	public static IErlSelection getWranglerSelection() {
		return wranglerSelection;
	}

	// TODO:: if the module is selected it is not handled
	public static void setSelection(ISelection selection) {
		if (editor == null) {
			Workbench instance = Workbench.getInstance();
			IWorkbenchWindow activeWorkbenchWindow = instance
					.getActiveWorkbenchWindow();
			editor = activeWorkbenchWindow.getActivePage().getActiveEditor();
		}
		if (selection instanceof ITextSelection) {

			wranglerSelection = new ErlTextMemberSelection(
					(ITextSelection) selection, (ITextEditor) editor);
		} else if (selection instanceof ITreeSelection) {
			Object firstElement = ((ITreeSelection) selection)
					.getFirstElement();
			if (firstElement instanceof IErlElement) {
				IErlElement element = (IErlElement) firstElement;
				IFile file = (IFile) element.getResource();
				wranglerSelection = new ErlMemberSelection(element, file,
						WranglerUtils.getDocument(file));
			} else if (firstElement instanceof IFile) {
				IFile file = (IFile) firstElement;
				IErlModule module = ErlangCore.getModel().findModule(file);
				wranglerSelection = new ErlModuleSelection(module, file);
			} else {
				wranglerSelection = null;
			}
		} else {
			wranglerSelection = null;
		}

		/*
		 * System.out.println(wranglerSelection.getStartLine() + "," +
		 * wranglerSelection.getStartCol() + ";" +
		 * wranglerSelection.getEndLine() + "," +
		 * wranglerSelection.getEndCol());
		 */

	}

	public static boolean showDecidableQuestion(Shell s, String question,
			String title) {
		boolean b;
		try {
			MessageBox mb = new MessageBox(s, SWT.ICON_WARNING | SWT.YES
					| SWT.NO);
			mb.setMessage(question);
			mb.setText(title);
			int response = mb.open();
			if (response == SWT.YES) {
				b = true;
			} else {
				b = false;
			}
			return b;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return true;
	}
}
