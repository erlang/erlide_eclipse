package org.erlide.ui.actions;

import java.util.ResourceBundle;

import org.eclipse.swt.SWT;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;
import org.erlide.ui.editors.outline.QuickOutlinePopupDialog;

public class ShowOutlineAction extends TextEditorAction {

	public ShowOutlineAction(ResourceBundle bundle, String prefix,
			ITextEditor editor) {
		super(bundle, prefix, editor);
	}

	@Override
	public void run() {
		new QuickOutlinePopupDialog(getTextEditor().getSite().getShell(),
				SWT.NONE, null, null).setVisible(true);
	}
}
