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
import org.erlide.core.model.root.IErlElement.Kind;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;

/**
 * Abstract class for representing Erlang member selection
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public abstract class AbstractErlMemberSelection extends AbstractErlSelection
        implements IErlMemberSelection {

    protected ITextSelection textSelection;

    protected IDocument document;

    /**
     * Default constructor
     */
    public AbstractErlMemberSelection() {
    }

    @Override
    public IDocument getDocument() {
        return document;
    }

    /**
     * Constructor
     * 
     * @param editor
     *            the erlang editor which is used to get the current selection
     */
    public AbstractErlMemberSelection(final ITextEditor editor) {
        final ITextSelection selection = (ITextSelection) editor
                .getSelectionProvider().getSelection();
        final IFileEditorInput input = (IFileEditorInput) editor
                .getEditorInput();
        document = editor.getDocumentProvider().getDocument(input);
        final IFile afile = input.getFile();
        store(selection, afile, document);
    }

    protected void store(final ITextSelection selection, final IFile afile,
            final IDocument adocument) {
        file = afile;
        textSelection = selection;
    }

    /**
     * @Override public ITextEditor getEditor() { return editor; }
     */

    @Override
    public SelectionKind getKind() {
        final Kind k = getErlElement().getKind();
        if (k == Kind.CLAUSE) {
            return SelectionKind.FUNCTION_CLAUSE;
        } else if (k == Kind.FUNCTION) {
            return SelectionKind.FUNCTION;
        } else {
            return SelectionKind.MODULE;
        }
    }
}
