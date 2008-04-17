package org.erlide.ui.actions;

import java.util.ResourceBundle;

import org.eclipse.swt.SWT;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.outline.QuickOutlinePopupDialog;

public class ShowOutlineAction extends TextEditorAction {

	public ShowOutlineAction(ResourceBundle bundle, String prefix,
			ITextEditor editor) {
		super(bundle, prefix, editor);
	}

	@Override
	public void run() {
		if (getTextEditor() instanceof ErlangEditor) {
			ErlangEditor editor = (ErlangEditor) getTextEditor();
			QuickOutlinePopupDialog quickOutlinePopupDialog = new QuickOutlinePopupDialog(
					getTextEditor().getSite().getShell(), SWT.NONE, editor,
					editor);
			quickOutlinePopupDialog.setSize(400, 200);
			quickOutlinePopupDialog.setVisible(true);
		}
	}
}
