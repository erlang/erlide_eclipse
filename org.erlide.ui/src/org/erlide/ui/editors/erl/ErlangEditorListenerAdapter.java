package org.erlide.ui.editors.erl;

import java.util.ListResourceBundle;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.IEditorInput;
import org.erlide.ui.util.ErlideSelection;

public class ErlangEditorListenerAdapter implements IErlangEditorListener {

	public void handleCursorPositionChanged(ErlangEditor edit,
			ErlideSelection ps) {
	}

	public void onCreateActions(ListResourceBundle resources,
			ErlangEditor edit, IProgressMonitor monitor) {
	}

	public void onDispose(ErlangEditor edit, IProgressMonitor monitor) {
	}

	public void onInputChanged(ErlangEditor edit, IEditorInput oldInput,
			IEditorInput input, IProgressMonitor monitor) {
	}

	public void onSave(ErlangEditor edit, IProgressMonitor monitor) {
	}

	public void onSetDocument(IDocument document, ErlangEditor edit,
			IProgressMonitor monitor) {
	}

}
