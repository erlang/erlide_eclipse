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
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.core.CoreScope;
import org.erlide.core.model.erlang.IErlMember;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlElement;
import org.erlide.wrangler.refactoring.backend.SyntaxInfo;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
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
     */
    public ErlTextMemberSelection(final ITextSelection selection,
            final ITextEditor editor) {
        final IFileEditorInput input = (IFileEditorInput) editor
                .getEditorInput();
        document = editor.getDocumentProvider().getDocument(input);
        final IFile theFile = input.getFile();
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

    public IErlElement getErlElement() {
        final IErlModule module = (IErlModule) CoreScope.getModel()
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

    public IErlRange getSelectionRange() {
        return new ErlRange(getStartLine(), getStartCol(), getEndLine(),
                getEndCol(), textSelection.getOffset(),
                textSelection.getLength());
    }

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

    public IErlModule getErlModule() {
        return (IErlModule) CoreScope.getModel().findElement(file);
    }
}
