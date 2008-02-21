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

import erlang.ErlideSupport;

public class ErlangTextEditorAction extends TextEditorAction {
	protected String fErlModule;

	protected String fErlFunction;

	public ErlangTextEditorAction(ResourceBundle bundle, String prefix,
			ITextEditor editor, String erlModule, String erlFunction) {
		super(bundle, prefix, editor);
		fErlModule = erlModule;
		fErlFunction = erlFunction;
	}

	@Override
	public void run() {
		super.run();
		final ISelection sel = getSelection();
		if (sel == null || sel.isEmpty() || !(sel instanceof ITextSelection)) {
			return;
		}
		final ITextSelection selection = (ITextSelection) sel;
		final int offset = selection.getOffset();
		int length = selection.getLength();
		final IDocument document = getTextEditor().getDocumentProvider()
				.getDocument(getTextEditor().getEditorInput());
		if (length == 0 && offset + length < document.getLength()) {
			length = 1;
		}
		final int end = offset + length;
		try {
			final String text = document.get(0, offset + length);
			final OtpErlangObject r1 = callErlang(selection, text);
			if (!((r1 instanceof OtpErlangString) || (r1 instanceof OtpErlangList && ((OtpErlangList) r1)
					.arity() == 0))) {
				final String e = r1.toString();
				ErlangPlugin.log(new Status(IStatus.ERROR,
						ErlangPlugin.PLUGIN_ID,
						IErlangStatusConstants.INTERNAL_ERROR,
						"IndentAction_error_message" + e, null));

				ErrorDialog.openError(getTextEditor().getSite().getShell(),
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
			final int firstLine = document.getLineOfOffset(offset);
			final int firstLineOffset = document.getLineOffset(firstLine);
			final int lastLine = document.getLineOfOffset(end);
			// final int lastLineOffset = document.getLineOffset(lastLine);
			final int nLines = lastLine - firstLine + 1;
			final Runnable runnable = new Runnable() {
				@SuppressWarnings("synthetic-access")
				public void run() {
					IRewriteTarget target = (IRewriteTarget) getTextEditor()
							.getAdapter(IRewriteTarget.class);
					if (target != null) {
						target.beginCompoundChange();
						if (nLines > 1) {
							target.setRedraw(false);
						}
					}
					try {
						// ErlLogger.debug("'"+newText+"'");
						document.replace(firstLineOffset,
								end - firstLineOffset, newText);
						selectAndReveal(firstLineOffset, newText.length());
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
				final Display display = getTextEditor().getEditorSite()
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
		final OtpErlangObject r1 = ErlideSupport.call(fErlModule, fErlFunction,
				selection, text);
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
			final ErlangEditor erlEditor = ((ErlangEditor) editor);
			erlEditor.selectAndReveal(newOffset, newLength);
		} else {
			// this is too intrusive, but will never get called anyway
			getTextEditor().selectAndReveal(newOffset, newLength);
		}

	}

}
