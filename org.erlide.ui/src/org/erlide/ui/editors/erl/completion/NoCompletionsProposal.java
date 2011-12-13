/*******************************************************************************
 * Copyright (c) 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.editors.erl.completion;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;

/**
 * Proposal telling that there are no proposals available.
 * <p>
 * Applying this proposal does nothing.
 * </p>
 * 
 */
public final class NoCompletionsProposal implements ICompletionProposal {

    /*
     * @see
     * org.eclipse.jface.text.contentassist.ICompletionProposal#apply(org.eclipse
     * .jface.text.IDocument)
     */
    @Override
    public void apply(final IDocument document) {
        // do nothing
    }

    /*
     * @seeorg.eclipse.jface.text.contentassist.ICompletionProposal#
     * getAdditionalProposalInfo()
     */
    @Override
    public String getAdditionalProposalInfo() {
        return null;
    }

    /*
     * @seeorg.eclipse.jface.text.contentassist.ICompletionProposal#
     * getContextInformation()
     */
    @Override
    public IContextInformation getContextInformation() {
        return null;
    }

    /*
     * @see
     * org.eclipse.jface.text.contentassist.ICompletionProposal#getDisplayString
     * ()
     */
    @Override
    public String getDisplayString() {
        return "No suggestions available";
    }

    /*
     * @see org.eclipse.jface.text.contentassist.ICompletionProposal#getImage()
     */
    @Override
    public Image getImage() {
        return null;
    }

    /*
     * @see
     * org.eclipse.jface.text.contentassist.ICompletionProposal#getSelection
     * (org.eclipse.jface.text.IDocument)
     */
    @Override
    public Point getSelection(final IDocument document) {
        return null;
    }

}
