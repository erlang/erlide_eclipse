package org.erlide.wrangler.refactoring.selection.internal;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.core.erlang.IErlElement.Kind;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;

public abstract class AbstractErlMemberSelection extends AbstractErlSelection
		implements IErlMemberSelection {

	protected ITextSelection textSelection;
	protected IDocument document;

	public AbstractErlMemberSelection() {
	}

	@Override
	public IDocument getDocument() {
		return document;
	}

	public AbstractErlMemberSelection(ITextEditor editor) {
		ITextSelection selection = (ITextSelection) (editor
				.getSelectionProvider().getSelection());
		IFileEditorInput input = (IFileEditorInput) editor.getEditorInput();
		document = editor.getDocumentProvider().getDocument(input);
		IFile file = input.getFile();
		store(selection, file, document);
	}

	protected void store(ITextSelection selection, IFile file,
			IDocument document) {
		this.file = file;
		textSelection = selection;
	}

	/**
	 * @Override public ITextEditor getEditor() { return editor; }
	 */

	@Override
	public SelectionKind getKind() {
		Kind k = getErlElement().getKind();
		if (k == Kind.CLAUSE)
			return SelectionKind.FUNCTION_CLAUSE;
		else if (k == Kind.FUNCTION)
			return SelectionKind.FUNCTION;
		else
			return SelectionKind.MODULE;
	}
}
