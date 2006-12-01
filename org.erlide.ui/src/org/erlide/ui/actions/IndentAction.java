package org.erlide.ui.actions;

import java.util.ResourceBundle;

import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.texteditor.ITextEditor;

/**
 * Our sample action implements workbench action delegate. The action proxy will
 * be created by the workbench and shown in the UI. When the user tries to use
 * the action, this delegate will be created and execution will be delegated to
 * it.
 * 
 * @see IWorkbenchWindowActionDelegate
 */

public class IndentAction extends ErlangTextEditorAction {

	public IndentAction(ResourceBundle bundle, String prefix, ITextEditor editor) {
		super(bundle, prefix, editor, "erlide_indent", "indent_lines");
	}

}