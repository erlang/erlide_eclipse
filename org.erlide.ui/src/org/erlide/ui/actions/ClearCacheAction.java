/**
 *
 */
package org.erlide.ui.actions;

import java.util.ResourceBundle;

import org.eclipse.ui.texteditor.TextEditorAction;
import org.erlide.ui.editors.erl.ErlangEditor;

/**
 * @author jakob
 * 
 */
public class ClearCacheAction extends TextEditorAction {

	private final ErlangEditor erlangEditor;

	public ClearCacheAction(final ResourceBundle bundle, final String prefix,
			final ErlangEditor erlangEditor) {
		super(bundle, prefix, erlangEditor);
		this.erlangEditor = erlangEditor;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.action.Action#run()
	 */
	@Override
	public void run() {
		erlangEditor.resetParser();
	}
}
