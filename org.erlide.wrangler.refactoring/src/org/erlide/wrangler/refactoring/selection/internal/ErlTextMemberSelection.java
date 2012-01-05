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
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.core.model.erlang.IErlMember;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlElement;
import org.erlide.wrangler.refactoring.backend.SyntaxInfo;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.erlide.wrangler.refactoring.exception.WranglerException;
import org.erlide.wrangler.refactoring.util.ErlRange;
import org.erlide.wrangler.refactoring.util.IErlRange;
import org.erlide.wrangler.refactoring.util.WranglerUtils;

/**
 * Selected Erlang member, from the editor
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class ErlTextMemberSelection extends AbstractErlMemberSelection {

    /**
     * Constructor
     * 
     * @param selection
     *            textselection from an Erlang editor
     * @param editor
     *            editor, where the text is selected
     * @throws WranglerException
     */
    public ErlTextMemberSelection(final ITextSelection selection,
            final ITextEditor editor) throws WranglerException {
        IEditorInput input = editor.getEditorInput();
        if (!(input instanceof IFileEditorInput))
            throw new WranglerException("Can not refactor external modules!");
        document = editor.getDocumentProvider().getDocument(input);
        final IFileEditorInput fileInput = (IFileEditorInput) input;
        IFile theFile = fileInput.getFile();
        store(selection, theFile, document);
    }

    /**
     * Constructor
     * 
     * @param editor
     *            editor, which contains the selection
     */
    public ErlTextMemberSelection(final ITextEditor editor) {
        super(editor);
    }

    protected int getStartCol() {
        return WranglerUtils.calculateColumnFromOffset(
                textSelection.getOffset(), getStartLine() - 1, document);
    }

    protected int getEndLine() {
        return textSelection.getEndLine() + 1;
    }

    protected int getEndCol() {
        return WranglerUtils.calculateColumnFromOffset(
                textSelection.getOffset() + textSelection.getLength(),
                getEndLine() - 1, document);
    }

    protected int getStartLine() {
        return textSelection.getStartLine() + 1;
    }

    @Override
    public IErlElement getErlElement() {
        final IErlModule module = (IErlModule) ErlModelManager.getErlangModel()
                .findElement(file);

        try {
            final IErlElement element = module.getElementAt(textSelection
                    .getOffset());
            if (element == null) {
                return module;
            } else {
                return element;
            }

        } catch (final ErlModelException e) {
        }
        return module;
    }

    @Override
    public IErlRange getMemberRange() {
        if (getErlElement() instanceof IErlMember) {
            IErlRange range = null;
            final IErlMember member = (IErlMember) getErlElement();
            int sL, sC, eL, eC;
            sL = member.getLineStart() + 1;
            eL = member.getLineEnd() + 1;

            sC = WranglerUtils.calculateColumnFromOffset(member
                    .getSourceRange().getOffset(), sL - 1, document);
            eC = WranglerUtils.calculateColumnFromOffset(member
                    .getSourceRange().getOffset()
                    + member.getSourceRange().getLength(), eL - 1, document);
            range = new ErlRange(sL, sC, eL, eC, member.getSourceRange()
                    .getOffset(), member.getSourceRange().getLength());

            return range;
        }
        return getSelectionRange();
    }

    @Override
    public IErlRange getSelectionRange() {
        return new ErlRange(getStartLine(), getStartCol(), getEndLine(),
                getEndCol(), textSelection.getOffset(),
                textSelection.getLength());
    }

    @Override
    public SelectionKind getDetailedKind() {
        if (getKind() == SelectionKind.FUNCTION
                || getKind() == SelectionKind.FUNCTION_CLAUSE) {
            final SyntaxInfo si = WranglerBackendManager.getSyntaxBackend()
                    .getSyntaxInfo(file, getStartLine(), getStartCol());
            if (si.isVariable()) {
                return SelectionKind.VARIABLE;
                // TODO:: expression checking is not implemented
            } else {
                return getKind();
            }
        } else {
            return getKind();
        }
    }

    @Override
    public IErlModule getErlModule() {
        return (IErlModule) ErlModelManager.getErlangModel().findElement(file);
    }
}
