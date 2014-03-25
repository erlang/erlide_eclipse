package org.erlide.ui.editors.erl.correction;

/*******************************************************************************
 * Copyright (c) 2000, 2008 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.IMarkerResolution2;

/**
  */
public class MarkerResolutionProposal implements ICompletionProposal {

    private final IMarkerResolution fResolution;
    private final IMarker fMarker;

    /**
     * Constructor for MarkerResolutionProposal.
     */
    public MarkerResolutionProposal(final IMarkerResolution resolution,
            final IMarker marker) {
        fResolution = resolution;
        fMarker = marker;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.text.contentassist.ICompletionProposal#apply(org.eclipse
     * .jface.text.IDocument)
     */
    @Override
    public void apply(final IDocument document) {
        fResolution.run(fMarker);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.contentassist.ICompletionProposal#
     * getAdditionalProposalInfo()
     */
    @Override
    public String getAdditionalProposalInfo() {
        if (fResolution instanceof IMarkerResolution2) {
            return ((IMarkerResolution2) fResolution).getDescription();
        }
        if (fResolution instanceof ICompletionProposal) {
            return ((ICompletionProposal) fResolution).getAdditionalProposalInfo();
        }
        try {
            final String problemDesc = (String) fMarker.getAttribute(IMarker.MESSAGE);
            return problemDesc;
            // return Messages.format(
            // CorrectionMessages.MarkerResolutionProposal_additionaldesc,
            // problemDesc);
        } catch (final CoreException e) {
            // JavaScriptPlugin.log(e);
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.contentassist.ICompletionProposal#
     * getContextInformation()
     */
    @Override
    public IContextInformation getContextInformation() {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.text.contentassist.ICompletionProposal#getDisplayString
     * ()
     */
    @Override
    public String getDisplayString() {
        return fResolution.getLabel();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.contentassist.ICompletionProposal#getImage()
     */
    @Override
    public Image getImage() {
        if (fResolution instanceof IMarkerResolution2) {
            return ((IMarkerResolution2) fResolution).getImage();
        }
        if (fResolution instanceof ICompletionProposal) {
            return ((ICompletionProposal) fResolution).getImage();
        }
        return null; // JavaPluginImages.get(JavaPluginImages.IMG_CORRECTION_CHANGE);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.text.contentassist.ICompletionProposal#getSelection
     * (org.eclipse.jface.text.IDocument)
     */
    @Override
    public Point getSelection(final IDocument document) {
        if (fResolution instanceof ICompletionProposal) {
            return ((ICompletionProposal) fResolution).getSelection(document);
        }
        return null;
    }

}
