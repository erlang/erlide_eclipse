package org.erlide.ui.actions;

import java.util.Map;
import java.util.ResourceBundle;

import org.eclipse.jface.text.ITextSelection;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditorPreferenceConstants;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IBackend;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.prefs.plugin.IndentationPreferencePage;

import com.ericsson.otp.erlang.OtpErlangObject;

import erlang.ErlideIndent;

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
		super(bundle, prefix, editor);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.ui.actions.ErlangTextEditorAction#callErlang(org.eclipse.jface.text.ITextSelection,
	 *      java.lang.String)
	 */
	@Override
	protected OtpErlangObject callErlang(ITextSelection selection, String text)
			throws Exception {
		int tabw = ErlideUIPlugin
				.getDefault()
				.getPreferenceStore()
				.getInt(
						AbstractDecoratedTextEditorPreferenceConstants.EDITOR_TAB_WIDTH);
		if (tabw == 0) {
			tabw = EditorsUI
					.getPreferenceStore()
					.getInt(
							AbstractDecoratedTextEditorPreferenceConstants.EDITOR_TAB_WIDTH);
		}

		final Map<String, String> prefs = IndentationPreferencePage
				.getKeysAndPrefs();
		final IBackend b = BackendManager.getDefault().getIdeBackend();
		final OtpErlangObject r1 = ErlideIndent.indentLines(b, selection
				.getOffset(), text, tabw, prefs);
		return r1;
	}

}