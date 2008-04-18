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
import org.erlide.basicui.util.IErlangStatusConstants;
import org.erlide.core.ErlangPlugin;
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

	public ErlangTextEditorAction(ResourceBundle bundle, String prefix,
			ITextEditor editor, String erlModule, String erlFunction) {
		super(bundle, prefix, editor);
		fErlModule = erlModule;
		fErlFunction = erlFunction;
	}

	public ErlangTextEditorAction(ResourceBundle bundle, String prefix,
			ITextEditor editor) {
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
						IErlangStatusConstants.INTERNAL_ERROR,
						"IndentAction_error_message" + e, null));

				ErrorDialog.openError(textEditor.getSite().getShell(),
						ActionMessages.IndentAction_error_message, e, null);
				return;
			}

			final String newText;
			if (r1 instanceof OtpErlangList) {
				newText = "";
			} else {
				final OtpErlangString s1 = (OtpErlangString) r1;
				newText = s1.stringValue();
			}
			// final int lastLineOffset = document.getLineOffset(lastLine);
			final int nLines = endLine - startLine + 1;
			final Runnable runnable = new Runnable() {
				@SuppressWarnings("synthetic-access")
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
						// TODO Auto-generated catch block
						e.printStackTrace();
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
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	/**
	 * @param selection
	 * @param text
	 * @return
	 * @throws BackendException
	 * @throws ErlangRpcException
	 */
	protected OtpErlangObject callErlang(ITextSelection selection, String text)
			throws Exception {
		final OtpErlangObject r1 = ErlideBackend.call(fErlModule, fErlFunction,
				selection.getOffset(), text);
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
	protected void selectAndReveal(int newOffset, int newLength) {
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
