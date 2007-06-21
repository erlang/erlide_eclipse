package org.erlide.ui.actions;

import java.util.ResourceBundle;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.text.Assert;
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
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.BackendUtil;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.ui.editors.erl.ErlangEditor;

import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

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
		final int length = selection.getLength();
		final int end = offset + length;
		final IDocument document = getTextEditor().getDocumentProvider()
				.getDocument(getTextEditor().getEditorInput());
		try {
			final String text = document.get(0, offset + length);
			final OtpErlangObject r1 = callErlang(selection, text);
			if (!(r1 instanceof OtpErlangString)) {
				final String e = r1.toString();
				ErlangPlugin.log(new Status(IStatus.ERROR,
						ErlangPlugin.PLUGIN_ID,
						IErlangStatusConstants.INTERNAL_ERROR,
						"IndentAction_error_message" + e, null));

				ErrorDialog.openError(getTextEditor().getSite().getShell(),
						ActionMessages.IndentAction_error_message, e, null);
				return;
			}
			final OtpErlangString s1 = (OtpErlangString) r1;
			final String newText = s1.stringValue();
			final int firstLine = document.getLineOfOffset(offset);
			final int firstLineOffset = document.getLineOffset(firstLine);
			final int lastLine = document.getLineOfOffset(end);
			// final int lastLineOffset = document.getLineOffset(lastLine);
			final int nLines = lastLine - firstLine + 1;
			final Runnable runnable = new Runnable() {
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
						// ErlLogger.log("'"+newText+"'");
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
		} catch (final BadLocationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final ErlangRpcException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final BackendException e) {
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
			throws BackendException, ErlangRpcException {
		final OtpErlangObject r1 = BackendUtil.checkRpc(BackendManager
				.getDefault().getIdeBackend().rpc(fErlModule, fErlFunction,
						new OtpErlangString(text),
						new OtpErlangLong(selection.getOffset())));
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
		Assert.isTrue(newOffset >= 0);
		Assert.isTrue(newLength >= 0);
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
