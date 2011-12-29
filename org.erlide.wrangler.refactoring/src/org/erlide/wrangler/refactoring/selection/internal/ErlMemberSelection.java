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
import org.erlide.core.model.erlang.IErlMember;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlElement;
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
        return WranglerUtils.calculateColumnFromOffset(member.getSourceRange()
                .getOffset() + member.getSourceRange().getLength(),
                getEndLine() - 1, document);
    }

    protected int getEndLine() {
        return member.getLineEnd() + 1;
    }

    @Override
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

    @Override
    public IErlRange getMemberRange() {
        return getSelectionRange();
    }

    @Override
    public IErlRange getSelectionRange() {
        IErlRange range;
        try {
            range = new ErlRange(getStartLine(), getStartCol(), getEndLine(),
                    getEndCol(), member.getSourceRange().getOffset(), member
                            .getSourceRange().getLength());
        } catch (final ErlModelException e) {
            e.printStackTrace();
            return null;
        }
        return range;
    }

    @Override
    public SelectionKind getDetailedKind() {
        return getKind();
    }

    @Override
    public IErlModule getErlModule() {
        return (IErlModule) ErlModelManager.getErlangModel().findElement(file);
    }
}
