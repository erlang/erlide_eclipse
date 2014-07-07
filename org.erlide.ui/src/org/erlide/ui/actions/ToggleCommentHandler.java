package org.erlide.ui.actions;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextOperationTarget;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.core.ErlangCore;
import org.erlide.core.ErlangStatus;
import org.erlide.engine.ErlangEngine;
import org.erlide.ui.handlers.ErlangAbstractHandler;
import org.erlide.util.ErlLogger;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangObject;

public class ToggleCommentHandler extends ErlangAbstractHandler {

    @Override
    protected void doAction(final ISelection sel, final ITextEditor textEditor) {
        final IDocument document = textEditor.getDocumentProvider().getDocument(
                textEditor.getEditorInput());
        final ITextSelection selection = extendSelectionToWholeLines(document,
                (ITextSelection) sel);
        final ITextSelection getSelection = getTextSelection(document, selection,
                textEditor);
        String text;
        OtpErlangObject r1 = null;
        try {
            text = document.get(getSelection.getOffset(), getSelection.getLength());
            // call erlang, with selection within text
            r1 = callErlang(selection.getOffset() - getSelection.getOffset(),
                    selection.getLength(), text);
        } catch (final Exception e) {
            ErlLogger.error(e);
        }
        final String newText = Util.stringValue(r1);
        if (newText == null) {
            final String msg = "call to " + getClass().getSimpleName()
                    + " timed out; try a smaller selection.";
            final Status status = new Status(IStatus.ERROR, ErlangCore.PLUGIN_ID,
                    ErlangStatus.INTERNAL_ERROR.getValue(), msg, null);
            ErlLogger.error("INTERNAL ERROR: " + msg);

            ErrorDialog.openError(textEditor.getSite().getShell(),
                    ActionMessages.IndentAction_error_message, "Internal error", status);
            return;
        }
        final Display display = textEditor.getEditorSite().getShell().getDisplay();
        display.syncExec(new Runnable() {
            @Override
            public void run() {
                final ITextOperationTarget target1 = (ITextOperationTarget) textEditor
                        .getAdapter(ITextOperationTarget.class);
                if (target1 instanceof ITextViewer) {
                    final ITextViewer textViewer = (ITextViewer) target1;
                    try {
                        if (!document.get(selection.getOffset(), selection.getLength())
                                .equals(newText)) {
                            document.replace(selection.getOffset(),
                                    selection.getLength(), newText);
                            textViewer.setSelectedRange(selection.getOffset(),
                                    newText.length());
                        }
                    } catch (final BadLocationException e) {
                        ErlLogger.warn(e);
                    }
                }
            }
        });
    }

    private OtpErlangObject callErlang(final int offset, final int length,
            final String aText) {
        final String fErlModule = "erlide_comment";
        final String fErlFunction = "toggle_comment";
        final OtpErlangObject r1 = ErlangEngine.getInstance().getGenericService()
                .call(fErlModule, fErlFunction, offset, length, aText);
        return r1;
    }
}
