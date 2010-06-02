/*******************************************************************************
 * Copyright (c) 2010 György Orosz.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     György Orosz - initial API and implementation
 ******************************************************************************/
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

	public IDocument getDocument() {
		return document;
	}

	public AbstractErlMemberSelection(ITextEditor editor) {
		ITextSelection selection = (ITextSelection) (editor
				.getSelectionProvider().getSelection());
		IFileEditorInput input = (IFileEditorInput) editor.getEditorInput();
		document = editor.getDocumentProvider().getDocument(input);
		IFile afile = input.getFile();
		store(selection, afile, document);
	}

	protected void store(ITextSelection selection, IFile afile,
			IDocument adocument) {
		this.file = afile;
		textSelection = selection;
	}

	/**
	 * @Override public ITextEditor getEditor() { return editor; }
	 */

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
