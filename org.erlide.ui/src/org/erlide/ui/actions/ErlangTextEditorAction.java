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
import org.erlide.backend.Backend;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.ErlangStatusConstants;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.erlang.ISourceReference;
import org.erlide.core.util.Util;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.util.ErlModelUtils;

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

	/**
	 * Extend the selection that the action will work on. Default
	 * implementation, extend to whole lines. Might be overridden.
	 * 
	 * @param document
	 *            text {@link IDocument}
	 * @param selection
	 *            original selection
	 * @return new {@link ITextSelection} extended to the whole lines
	 *         intersected by selection
	 */
	protected ITextSelection extendSelection(final IDocument document,
			final ITextSelection selection) {
		final int startLine = selection.getStartLine();
		final int endLine = selection.getEndLine();
		int startLineOffset;
		try {
			startLineOffset = document.getLineOffset(startLine);
			final int endTextOffset = document.getLineOffset(endLine)
					+ document.getLineLength(endLine);
			return new TextSelection(document, startLineOffset, endTextOffset
					- startLineOffset);
		} catch (final BadLocationException e) {
			e.printStackTrace();
		}
		return selection;
	}

	/**
	 * Provide the text selection that is needed to execute the command. Default
	 * implementation, extend to Erlang elements selected.
	 * 
	 * @param document
	 *            text {@link IDocument}
	 * @param selection
	 *            selection affected by command (extended by extendSelection)
	 * @return new {@link ITextSelection} with all text up to selection
	 */
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
					} else if (e2 == null) {
						return extendSelection(document, new TextSelection(
								document, r1.getOffset(), selection.getLength()
										+ selection.getOffset()
										- r1.getOffset()));
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
		return extendSelection(document, selection);
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
		final String newText = Util.stringValue(r1);
		if (newText == null) {
			final Status status = new Status(IStatus.ERROR,
					ErlangPlugin.PLUGIN_ID,
					ErlangStatusConstants.INTERNAL_ERROR, "indent returned "
							+ r1 + " instead of a string", null);
			ErlLogger.error("INTERNAL ERROR: indent returned " + r1
					+ " instead of a string");

			ErrorDialog.openError(textEditor.getSite().getShell(),
					ActionMessages.IndentAction_error_message, String
							.valueOf(r1), status);
			return;
		}
		final int startLine = selection.getStartLine();
		final int endLine = selection.getEndLine();
		final int nLines = endLine - startLine + 1;
		final Runnable runnable = new Runnable() {
			public void run() {
				final IRewriteTarget target = (IRewriteTarget) textEditor
						.getAdapter(IRewriteTarget.class);
				if (target != null) {
					target.beginCompoundChange();
					if (nLines > 1) {
						target.setRedraw(false);
					}
				}
				try {
					// ErlLogger.debug("'"+newText+"'");
					if (!document.get(selection.getOffset(),
							selection.getLength()).equals(newText)) {
						document.replace(selection.getOffset(), selection
								.getLength(), newText);
					}
					selectAndReveal(selection.getOffset(), newText.length());
				} catch (final BadLocationException e) {
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
	 */
	protected OtpErlangObject callErlang(final int offset, final int length,
			final String aText) throws BackendException {
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
