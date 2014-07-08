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
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.erlang.IErlMember;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlElement;
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
                .getOffset() + member.getSourceRange().getLength(), getEndLine() - 1,
                document);
    }

    protected int getEndLine() {
        return member.getLineEnd() + 1;
    }

    @Override
    public IErlElement getErlElement() {
        return element;
    }

    protected int getStartCol() {
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
        range = new ErlRange(getStartLine(), getStartCol(), getEndLine(), getEndCol(),
                member.getSourceRange().getOffset(), member.getSourceRange().getLength());
        return range;
    }

    @Override
    public SelectionKind getDetailedKind() {
        return getKind();
    }

    @Override
    public IErlModule getErlModule() {
        return (IErlModule) ErlangEngine.getInstance().getModel().findElement(file);
    }
}
