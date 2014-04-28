package org.erlide.ui.actions;

import java.util.Map;
import java.util.TreeMap;

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
import org.erlide.core.ErlangCore;
import org.erlide.core.ErlangStatus;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.services.text.IndentService;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.ui.editors.erl.autoedit.AutoIndentStrategy;
import org.erlide.ui.editors.erl.autoedit.SmartTypingPreferencePage;
import org.erlide.ui.handlers.ErlangAbstractHandler;
import org.erlide.ui.prefs.plugin.IndentationPreferencePage;
import org.erlide.util.ErlLogger;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangObject;

public class IndentHandler extends ErlangAbstractHandler {

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
                    if (!document.get(selection.getOffset(), selection.getLength())
                            .equals(newText)) {
                        document.replace(selection.getOffset(), selection.getLength(),
                                newText);
                    }
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
            final String text) throws RpcException {
        final OtpErlangObject r1 = doIndentLines(offset, length, text, false, "");
        return r1;
    }

    protected static OtpErlangObject doIndentLines(final int offset, final int length,
            final String text, final boolean template, final String prefix)
            throws RpcException {

        final IndentService indentService = ErlangEngine.getInstance().getService(
                IndentService.class);

        final int tabw = AutoIndentStrategy.getTabWidthFromPreferences();
        final Map<String, String> prefs = new TreeMap<String, String>();
        IndentationPreferencePage.addKeysAndPrefs(prefs);
        SmartTypingPreferencePage.addAutoNLKeysAndPrefs(prefs);
        final boolean useTabs = AutoIndentStrategy.getUseTabsFromPreferences();
        if (template) {
            final OtpErlangObject r1 = indentService.templateIndentLines(prefix, text,
                    tabw, useTabs, prefs);
            return r1;
        }
        final OtpErlangObject r1 = indentService.indentLines(offset, length, text, tabw,
                useTabs, prefs);
        return r1;
    }

    public static String indentLines(final int offset, final int length,
            final String text, final boolean template, final String prefix)
            throws RpcException {
        return Util.stringValue(doIndentLines(offset, length, text, template, prefix));
    }

}
