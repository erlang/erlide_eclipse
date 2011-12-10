/*******************************************************************************
 * Copyright (c) 2009 * and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     *
 *******************************************************************************/
package org.erlide.ui.editors.erl.completion;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposalExtension4;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;

public class DummyCompletionProposal implements ICompletionProposal,
        ICompletionProposalExtension4 {

    CompletionProposal delegate;

    public DummyCompletionProposal(final int offset) {
        delegate = new CompletionProposal("", 0, 0, offset, null,
                "No completion proposal available", null, null);
    }

    @Override
    public void apply(final IDocument document) {
        delegate.apply(document);
    }

    @Override
    public Point getSelection(final IDocument document) {
        return delegate.getSelection(document);
    }

    @Override
    public String getAdditionalProposalInfo() {
        return delegate.getAdditionalProposalInfo();
    }

    @Override
    public String getDisplayString() {
        return delegate.getDisplayString();
    }

    @Override
    public Image getImage() {
        return delegate.getImage();
    }

    @Override
    public IContextInformation getContextInformation() {
        return delegate.getContextInformation();
    }

    @Override
    public boolean isAutoInsertable() {
        return false;
    }

}
