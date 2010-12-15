package org.erlide.ui.editors.erl;

import java.util.ListResourceBundle;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.IEditorInput;
import org.erlide.ui.util.ErlideSelection;

public class ErlangEditorListenerAdapter implements IErlangEditorListener {

    public void handleCursorPositionChanged(final ErlangEditor edit,
            final ErlideSelection ps) {
    }

    public void onCreateActions(final ListResourceBundle resources,
            final ErlangEditor edit, final IProgressMonitor monitor) {
    }

    public void onDispose(final ErlangEditor edit,
            final IProgressMonitor monitor) {
    }

    public void onInputChanged(final ErlangEditor edit,
            final IEditorInput oldInput, final IEditorInput input,
            final IProgressMonitor monitor) {
    }

    public void onSave(final ErlangEditor edit, final IProgressMonitor monitor) {
    }

    public void onSetDocument(final IDocument document,
            final ErlangEditor edit, final IProgressMonitor monitor) {
    }

}
