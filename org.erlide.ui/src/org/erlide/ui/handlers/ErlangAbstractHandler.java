package org.erlide.ui.handlers;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.ITextEditorExtension;
import org.eclipse.ui.texteditor.ITextEditorExtension2;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.ISourceRange;
import org.erlide.engine.model.erlang.ISourceReference;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.ui.editors.erl.AbstractErlangEditor;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.util.ErlLogger;
import org.erlide.util.event_tracer.ErlideEventTracer;

public abstract class ErlangAbstractHandler extends AbstractHandler {

    protected boolean validateEditorInputState(final ITextEditor editor) {
        if (editor instanceof ITextEditorExtension2) {
            return ((ITextEditorExtension2) editor).validateEditorInputState();
        } else if (editor instanceof ITextEditorExtension) {
            return !((ITextEditorExtension) editor).isEditorInputReadOnly();
        } else if (editor != null) {
            return editor.isEditable();
        } else {
            return false;
        }
    }

    /**
     * Selects the given range on the editor.
     *
     * @param newOffset
     *            the selection offset
     * @param newLength
     *            the selection range
     */
    protected void selectAndReveal(final int newOffset, final int newLength,
            final ITextEditor editor) {
        if (editor instanceof ErlangEditor) {
            final AbstractErlangEditor erlEditor = (AbstractErlangEditor) editor;
            erlEditor.selectAndReveal(newOffset, newLength);
        } else {
            // this is too intrusive, but will never get called anyway
            editor.selectAndReveal(newOffset, newLength);
        }

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
            final ITextSelection selection, final ITextEditor editor) {
        if (editor instanceof ErlangEditor) {
            final AbstractErlangEditor erlangEditor = (AbstractErlangEditor) editor;
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
                                    new TextSelection(document, offset, selection
                                            .getLength() + selection.getOffset() - offset));
                        } else if (e2 instanceof ISourceReference) {
                            final ISourceReference ref2 = (ISourceReference) e2;
                            final ISourceRange r2 = ref2.getSourceRange();
                            return extendSelectionToWholeLines(document,
                                    new TextSelection(document, offset, r2.getOffset()
                                            - offset + r2.getLength()));
                        }
                    }
                } catch (final ErlModelException e) {
                }
            }
        }
        return extendSelectionToWholeLines(document, selection);
    }

    @Override
    public Object execute(final ExecutionEvent event) throws ExecutionException {

        final ITextEditor textEditor = (ITextEditor) HandlerUtil.getActiveEditor(event);
        if (!validateEditorInputState(textEditor)) {
            return null;
        }
        final ISelection sel = textEditor.getSelectionProvider().getSelection();
        if (sel == null || sel.isEmpty() || !(sel instanceof ITextSelection)) {
            return null;
        }
        ErlideEventTracer.getInstance().traceOperationStart(this);
        try {
            final IRunnableWithProgress myRunnableWithProgress = new IRunnableWithProgress() {
                @Override
                public void run(final IProgressMonitor monitor0) {
                    final IProgressMonitor monitor = monitor0 != null ? monitor0
                            : new NullProgressMonitor();
                    try {
                        monitor.beginTask("Processing "
                                + textEditor.getEditorInput().getName(),
                                IProgressMonitor.UNKNOWN);
                        doAction(sel, textEditor);
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
        return null;
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
    public static ITextSelection extendSelectionToWholeLines(final IDocument document,
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
            ErlLogger.error(e);
        }
        return selection;
    }

    protected void doAction(final ISelection sel, final ITextEditor textEditor) {
        throw new AbstractMethodError();
    }

}
