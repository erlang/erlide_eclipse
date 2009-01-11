package org.erlide.ui.actions;

import java.util.ResourceBundle;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRewriteTarget;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.TextSelection;
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

import com.ericsson.otp.erlang.OtpErlangObject;

import erlang.ErlideIndent;

public class ErlangTextEditorAction extends TextEditorAction {
	final protected String fErlModule;
	final protected String fErlFunction;

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

	protected ITextSelection extendSelection(final IDocument document,
			final ITextSelection selection) {
		int startLine = selection.getStartLine();
		if (startLine == -1) {
			try {
				startLine = document.getLineOfOffset(selection.getOffset());
			} catch (final BadLocationException e) {
				return selection;
			}
		}
		int endLine = selection.getEndLine();
		if (endLine == -1) {
			try {
				endLine = document.getLineOfOffset(selection.getOffset()
						+ selection.getLength());
			} catch (final BadLocationException e) {
				return selection;
			}
		}
		int startLineOffset;
		try {
			startLineOffset = document.getLineOffset(startLine);
			final int endTextOffset = document.getLineOffset(endLine)
					+ document.getLineLength(endLine);
			return new TextSelection(document, startLineOffset, endTextOffset
					- startLineOffset);
		} catch (final BadLocationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return selection;
	}

	protected ITextSelection getTextSelection(final IDocument document,
			final ITextSelection selection) {
		return new TextSelection(0, selection.getOffset()
				+ selection.getLength());
	}

	@Override
	public void run() {
		super.run();
		final ISelection sel = getSelection();
		if (sel == null || sel.isEmpty() || !(sel instanceof ITextSelection)) {
			return;
		}
		final ITextEditor textEditor = getTextEditor();
		final IDocument document = textEditor.getDocumentProvider()
				.getDocument(textEditor.getEditorInput());
		final ITextSelection selection = extendSelection(document,
				(ITextSelection) sel);
		final ITextSelection getSelection = getTextSelection(document,
				selection);
		String text;
		OtpErlangObject r1 = null;
		try {
			text = document.get(getSelection.getOffset(), getSelection
					.getLength());
			// call erlang, with selection within text
			r1 = callErlang(selection.getOffset() - getSelection.getOffset(),
					selection.getLength(), text);
		} catch (final Exception e) {
			e.printStackTrace();
		}
		// if (r1 instanceof OtpErlangList) {
		// final OtpErlangList l = (OtpErlangList) r1;
		// for (int i = 0, n = l.arity(); i < n; ++i) {
		// OtpErlangObject o = l.elementAt(i);
		// if (o instanceof OtpErlangLong) {
		// final OtpErlangLong lo = (OtpErlangLong) o;
		// final int intValue = lo.intValue();
		// if (intValue < 0 || intValue > 255) {
		// int ix = intValue;
		// ix = -ix;
		// ix = -ix;
		// }
		// } else {
		// OtpErlangObject ix = o;
		// o = ix;
		// ix = o;
		// }
		// }
		// }
		final String newText = Util.stringValue(r1);
		if (newText == null) {
			final String e = r1.toString();
			ErlangPlugin.log(new Status(IStatus.ERROR, ErlangPlugin.PLUGIN_ID,
					ErlangStatusConstants.INTERNAL_ERROR,
					"IndentAction_error_message" + e, null));

			ErrorDialog.openError(textEditor.getSite().getShell(),
					ActionMessages.IndentAction_error_message, e, null);
			return;
		}
		final int startLine = selection.getStartLine();
		final int endLine = selection.getEndLine();
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
					document.replace(selection.getOffset(), selection
							.getLength(), newText);
					selectAndReveal(selection.getOffset(), newText.length());
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
	}

	/**
	 * @param length
	 * @param aSelection
	 * @param aText
	 * @return
	 * @throws BackendException
	 * @throws ErlangRpcException
	 */
	protected OtpErlangObject callErlang(final int offset, final int length,
			final String aText) throws Exception {
		final Backend b = ErlangCore.getBackendManager().getIdeBackend();
		final OtpErlangObject r1 = ErlideIndent.call(b, fErlModule,
				fErlFunction, offset, length, aText);
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
