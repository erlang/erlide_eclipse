package org.erlide.ui.editors.erl.actions;

import java.lang.reflect.InvocationTargetException;
import java.util.ResourceBundle;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRewriteTarget;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;
import org.erlide.backend.BackendUtils;
import org.erlide.core.ErlangCore;
import org.erlide.core.ErlangStatus;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.ISourceRange;
import org.erlide.engine.model.erlang.ISourceReference;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.ui.actions.ActionMessages;
import org.erlide.ui.editors.erl.AbstractErlangEditor;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.util.ErlLogger;
import org.erlide.util.Util;
import org.erlide.util.event_tracer.ErlideEventTracer;

import com.ericsson.otp.erlang.OtpErlangObject;

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
    public static ITextSelection extendSelectionToWholeLines(
            final IDocument document, final ITextSelection selection) {
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
            ErlLogger.error(e);
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
        if (getTextEditor() instanceof ErlangEditor) {
            final AbstractErlangEditor erlangEditor = (AbstractErlangEditor) getTextEditor();
            final IErlModule module = erlangEditor.getModule();
            if (module != null) {
                final int offset1 = selection.getOffset(), offset2 = offset1
                        + selection.getLength();
                try {
                    final IErlElement e1 = module.getElementAt(offset1);
                    final IErlElement e2 = module.getElementAt(offset2);
                    if (e1 instanceof ISourceReference) {
                        final ISourceReference ref1 = (ISourceReference) e1;
                        final ISourceRange r1 = ref1.getSourceRange();
                        final int offset = r1.getOffset();
                        int length = r1.getLength();
                        if (e1 == e2) {
                            final int docLength = document.getLength();
                            if (offset + length > docLength) {
                                length = docLength - offset;
                            }
                            return extendSelectionToWholeLines(document,
                                    new TextSelection(document, offset, length));
                        } else if (e2 == null) {
                            return extendSelectionToWholeLines(
                                    document,
                                    new TextSelection(document, offset,
                                            selection.getLength()
                                                    + selection.getOffset()
                                                    - offset));
                        } else if (e2 instanceof ISourceReference) {
                            final ISourceReference ref2 = (ISourceReference) e2;
                            final ISourceRange r2 = ref2.getSourceRange();
                            return extendSelectionToWholeLines(
                                    document,
                                    new TextSelection(document, offset, r2
                                            .getOffset()
                                            - offset
                                            + r2.getLength()));
                        }
                    }
                } catch (final ErlModelException e) {
                }
            }
        }
        return extendSelectionToWholeLines(document, selection);
    }

    @Override
    public void run() {
        final ISelection sel = getSelection();
        if (sel == null || sel.isEmpty() || !(sel instanceof ITextSelection)) {
            return;
        }
        if (!validateEditorInputState()) {
            return;
        }
        ErlideEventTracer.getInstance().traceOperationStart(this);
        try {
            final ITextEditor textEditor = getTextEditor();
            final IRunnableWithProgress myRunnableWithProgress = new IRunnableWithProgress() {
                @Override
                public void run(final IProgressMonitor monitor0) {
                    final IProgressMonitor monitor = monitor0 != null ? monitor0
                            : new NullProgressMonitor();
                    try {
                        monitor.beginTask("Indenting "
                                + textEditor.getEditorInput().getName(),
                                IProgressMonitor.UNKNOWN);
                        doIndent(sel);
                    } finally {
                        monitor.done();
                    }
                }
            };

            try {
                PlatformUI.getWorkbench().getProgressService()
                        .busyCursorWhile(myRunnableWithProgress);
            } catch (final InvocationTargetException e) {
            } catch (final InterruptedException e) {
            }
        } finally {
            ErlideEventTracer.getInstance().traceOperationEnd(this);
        }
    }

    /**
     * @param length
     * @param aSelection
     * @param aText
     * @return
     * @throws RpcException
     */
    protected OtpErlangObject callErlang(final int offset, final int length,
            final String aText) throws RpcException {
        final IRpcSite b = ErlangEngine.getInstance().getBackend();
        final OtpErlangObject r1 = BackendUtils.call(b, fErlModule,
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
            final AbstractErlangEditor erlEditor = (AbstractErlangEditor) editor;
            erlEditor.selectAndReveal(newOffset, newLength);
        } else {
            // this is too intrusive, but will never get called anyway
            getTextEditor().selectAndReveal(newOffset, newLength);
        }

    }

    private void doIndent(final ISelection sel) {
        final ITextEditor textEditor = getTextEditor();
        final IDocument document = textEditor.getDocumentProvider()
                .getDocument(textEditor.getEditorInput());
        final ITextSelection selection = extendSelectionToWholeLines(document,
                (ITextSelection) sel);
        final ITextSelection getSelection = getTextSelection(document,
                selection);
        String text;
        OtpErlangObject r1 = null;
        try {
            text = document.get(getSelection.getOffset(),
                    getSelection.getLength());
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
            final Status status = new Status(IStatus.ERROR,
                    ErlangCore.PLUGIN_ID,
                    ErlangStatus.INTERNAL_ERROR.getValue(), msg, null);
            ErlLogger.error("INTERNAL ERROR: " + msg);

            ErrorDialog.openError(textEditor.getSite().getShell(),
                    ActionMessages.IndentAction_error_message,
                    "Internal error", status);
            return;
        }
        final Display display = textEditor.getEditorSite().getShell()
                .getDisplay();
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
                    if (!document.get(selection.getOffset(),
                            selection.getLength()).equals(newText)) {
                        document.replace(selection.getOffset(),
                                selection.getLength(), newText);
                    }
                    selectAndReveal(selection.getOffset(), newText.length());
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
}
