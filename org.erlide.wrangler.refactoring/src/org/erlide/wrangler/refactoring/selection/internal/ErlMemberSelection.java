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
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlMember;
import org.erlide.wrangler.refactoring.util.ErlRange;
import org.erlide.wrangler.refactoring.util.IErlRange;
import org.erlide.wrangler.refactoring.util.WranglerUtils;

/**
 * Represents an Erlang member selection
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class ErlMemberSelection extends AbstractErlMemberSelection {
    protected IErlElement element;

    protected IErlMember member;

    /**
     * Constructor
     * 
     * @param element
     *            Erlang element - Erlide representation
     * @param file
     *            the file which contains the selection
     * @param document
     *            document which contains the selection
     */
    public ErlMemberSelection(final IErlElement element, final IFile file,
            final IDocument document) {
        this.document = document;
        this.file = file;
        this.element = element;
        if (element instanceof IErlMember) {
            member = (IErlMember) element;
        }
    }

    protected int getEndCol() {
        try {
            return WranglerUtils.calculateColumnFromOffset(member
                    .getSourceRange().getOffset()
                    + member.getSourceRange().getLength(), getEndLine() - 1,
                    document);
        } catch (ErlModelException e) {
            e.printStackTrace();
            return -1;
        }
    }

    protected int getEndLine() {
        return member.getLineEnd() + 1;
    }

    public IErlElement getErlElement() {
        return element;
    }

    protected int getStartCol() throws ErlModelException {
        return WranglerUtils.calculateColumnFromOffset(member.getSourceRange()
                .getOffset(), getStartLine() - 1, document);

    }

    protected int getStartLine() {
        return member.getLineStart() + 1;
    }

    public IErlRange getMemberRange() {
        return getSelectionRange();
    }

    public IErlRange getSelectionRange() {
        IErlRange range;
        try {
            range = new ErlRange(getStartLine(), getStartCol(), getEndLine(),
                    getEndCol(), member.getSourceRange().getOffset(), member
                            .getSourceRange().getLength());
        } catch (ErlModelException e) {
            e.printStackTrace();
            return null;
        }
        return range;
    }

    public SelectionKind getDetailedKind() {
        return getKind();
    }
}
