package org.erlide.ui.actions;

import java.util.ResourceBundle;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRewriteTarget;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.ErlangStatusConstants;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.util.Util;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.Backend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.ui.editors.erl.ErlangEditor;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

import erlang.ErlideBackend;

public class ErlangTextEditorAction extends TextEditorAction {
	final protected String fErlModule;
	final protected String fErlFunction;
	private String text;
	private ITextSelection selection;

	public ErlangTextEditorAction(final ResourceBundle bundle,
			final String prefix, final ITextEditor editor,
			final String erlModule, final String erlFunction) {
		super(bundle, prefix, editor);
		fErlModule = erlModule;
		fErlFunction = erlFunction;
	}

	public ErlangTextEditorAction(final ResourceBundle bundle,
			final String prefix, final ITextEditor editor) {
		this(bundle, prefix, editor, null, null);
	}

	@Override
	public void run() {
		super.run();
		final ISelection sel = getSelection();
		if (sel == null || sel.isEmpty() || !(sel instanceof ITextSelection)) {
			return;
		}
		selection = (ITextSelection) sel;
		final int startLine = selection.getStartLine();
		final int endLine = selection.getEndLine();
		final ITextEditor textEditor = getTextEditor();
		final IDocument document = textEditor.getDocumentProvider()
				.getDocument(textEditor.getEditorInput());
		try {
			final int startLineOffset = document.getLineOffset(startLine);
			final int endTextOffset = document.getLineOffset(endLine)
					+ document.getLineLength(endLine);
			text = document.get(0, endTextOffset);
			final OtpErlangObject r1 = callErlang(selection, text);
			if (!(r1 instanceof OtpErlangString || r1 instanceof OtpErlangList
					&& ((OtpErlangList) r1).arity() == 0)) {
				final String e = r1.toString();
				ErlangPlugin.log(new Status(IStatus.ERROR,
						ErlangPlugin.PLUGIN_ID,
						ErlangStatusConstants.INTERNAL_ERROR,
						"IndentAction_error_message" + e, null));

				ErrorDialog.openError(textEditor.getSite().getShell(),
						ActionMessages.IndentAction_error_message, e, null);
				return;
			}
			final String newText = Util.stringValue(r1);
			// final int lastLineOffset = document.getLineOffset(lastLine);
			final int nLines = endLine - startLine + 1;
			final Runnable runnable = new Runnable() {
				public void run() {
					IRewriteTarget target = (IRewriteTarget) textEditor
							.getAdapter(IRewriteTarget.class);
					if (target != null) {
						target.beginCompoundChange();
						if (nLines > 1) {
							target.setRedraw(false);
						}
					}
					try {
						// ErlLogger.debug("'"+newText+"'");
						document.replace(startLineOffset, endTextOffset
								- startLineOffset, newText);
						selectAndReveal(startLineOffset, newText.length());
					} catch (BadLocationException e) {
						ErlLogger.warn(e);
					}
					if (target != null) {
						target.endCompoundChange();
						if (nLines > 1) {
							target.setRedraw(true);
						}
					}
				}
			};

			if (nLines > 50) {
				final Display display = textEditor.getEditorSite()
						.getWorkbenchWindow().getShell().getDisplay();
				BusyIndicator.showWhile(display, runnable);
			} else {
				runnable.run();
			}
		} catch (final Exception e) {
			ErlLogger.warn(e);
		}

	}

	/**
	 * @param aSelection
	 * @param aText
	 * @return
	 * @throws BackendException
	 * @throws ErlangRpcException
	 */
	protected OtpErlangObject callErlang(final ITextSelection aSelection,
			final String aText) throws Exception {
		final Backend b = ErlangCore.getBackendManager().getIdeBackend();
		final OtpErlangObject r1 = ErlideBackend.call(b, fErlModule,
				fErlFunction, aSelection.getOffset(), aText);
		return r1;
	}

	protected ISelection getSelection() {
		return getTextEditor().getSelectionProvider().getSelection();
	}

	/**
	 * Selects the given range on the editor.
	 * 
	 * @param newOffset
	 *            the selection offset
	 * @param newLength
	 *            the selection range
	 */
	protected void selectAndReveal(final int newOffset, final int newLength) {
		final ITextEditor editor = getTextEditor();
		if (editor instanceof ErlangEditor) {
			final ErlangEditor erlEditor = (ErlangEditor) editor;
			erlEditor.selectAndReveal(newOffset, newLength);
		} else {
			// this is too intrusive, but will never get called anyway
			getTextEditor().selectAndReveal(newOffset, newLength);
		}

	}

}
