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

    public void apply(final IDocument document) {
        delegate.apply(document);
    }

    public Point getSelection(final IDocument document) {
        return delegate.getSelection(document);
    }

    public String getAdditionalProposalInfo() {
        return delegate.getAdditionalProposalInfo();
    }

    public String getDisplayString() {
        return delegate.getDisplayString();
    }

    public Image getImage() {
        return delegate.getImage();
    }

    public IContextInformation getContextInformation() {
        return delegate.getContextInformation();
    }

    public boolean isAutoInsertable() {
        return false;
    }

}
