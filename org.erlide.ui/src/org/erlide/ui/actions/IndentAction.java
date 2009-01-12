package org.erlide.ui.actions;

import java.util.Map;
import java.util.ResourceBundle;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditorPreferenceConstants;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.erlang.ISourceReference;
import org.erlide.runtime.backend.Backend;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.prefs.plugin.IndentationPreferencePage;
import org.erlide.ui.util.ErlModelUtils;

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

	public IndentAction(final ResourceBundle bundle, final String prefix,
			final ITextEditor editor) {
		super(bundle, prefix, editor);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.ui.actions.ErlangTextEditorAction#getTextSelection(org.eclipse.jface.text.IDocument,
	 *      org.eclipse.jface.text.ITextSelection)
	 */
	@Override
	protected ITextSelection getTextSelection(final IDocument document,
			final ITextSelection selection) {
		final IErlModule m = ErlModelUtils.getModule(getTextEditor());
		if (m != null) {
			final int offset1 = selection.getOffset(), offset2 = offset1
					+ selection.getLength();
			try {
				final IErlElement e1 = m.getElementAt(offset1);
				final IErlElement e2 = m.getElementAt(offset2);
				if (e1 instanceof ISourceReference) {
					final ISourceReference ref1 = (ISourceReference) e1;
					final ISourceRange r1 = ref1.getSourceRange();
					if (e1 == e2) {
						return extendSelection(document, new TextSelection(
								document, r1.getOffset(), r1.getLength()));
					} else if (e2 instanceof ISourceReference) {
						final ISourceReference ref2 = (ISourceReference) e2;
						final ISourceRange r2 = ref2.getSourceRange();
						return extendSelection(document, new TextSelection(
								document, r1.getOffset(), r2.getOffset()
										- r1.getOffset() + r2.getLength()));
					}
				}
			} catch (final ErlModelException e) {
			}
		}
		return super.getTextSelection(document, selection);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.ui.actions.ErlangTextEditorAction#callErlang(org.eclipse.jface
	 *      .text.ITextSelection, java.lang.String)
	 */
	@Override
	protected OtpErlangObject callErlang(final int offset, final int length,
			final String text) throws Exception {
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
		final Backend b = ErlangCore.getBackendManager().getIdeBackend();
		final OtpErlangObject r1 = ErlideIndent.indentLines(b, offset, length,
				text, tabw, prefs);
		return r1;
	}
}