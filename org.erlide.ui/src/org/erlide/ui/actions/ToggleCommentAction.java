package org.erlide.ui.actions;

import java.util.ResourceBundle;

import org.eclipse.ui.texteditor.ITextEditor;

public class ToggleCommentAction extends ErlangTextEditorAction {

	public ToggleCommentAction(ResourceBundle bundle, String prefix,
			ITextEditor editor) {
		super(bundle, prefix, editor, "erlide_comment", "toggle_comment");
	}

}
