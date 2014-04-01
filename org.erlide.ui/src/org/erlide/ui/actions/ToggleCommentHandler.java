package org.erlide.ui.actions;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRewriteTarget;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.backend.BackendUtils;
import org.erlide.core.ErlangCore;
import org.erlide.core.ErlangStatus;
import org.erlide.engine.ErlangEngine;
import org.erlide.runtime.api.IRpcSite;
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
                final IRewriteTarget target = (IRewriteTarget) textEditor
                        .getAdapter(IRewriteTarget.class);
                if (target != null) {
                    target.beginCompoundChange();
                    target.setRedraw(false);
                }
                try {
                    // ErlLogger.debug("'"+newText+"'");
                    if (!document.get(selection.getOffset(), selection.getLength())
                            .equals(newText)) {
                        document.replace(selection.getOffset(), selection.getLength(),
                                newText);
                    }
                    selectAndReveal(selection.getOffset(), newText.length(), textEditor);
                } catch (final BadLocationException e) {
                    ErlLogger.warn(e);
                }
                if (target != null) {
                    target.endCompoundChange();
                    target.setRedraw(true);
                }
            }
        });
    }

    private OtpErlangObject callErlang(final int offset, final int length,
            final String aText) {
        final IRpcSite b = ErlangEngine.getInstance().getBackend();
        final String fErlModule = "erlide_comment";
        final String fErlFunction = "toggle_comment";
        final OtpErlangObject r1 = BackendUtils.call(b, fErlModule, fErlFunction, offset,
                length, aText);
        return r1;
    }
}
