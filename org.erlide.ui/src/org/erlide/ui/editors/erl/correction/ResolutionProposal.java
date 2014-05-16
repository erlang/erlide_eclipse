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

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.IMarkerResolution2;

/**
 */
public class ResolutionProposal implements ICompletionProposal {

    protected final IMarkerResolution fResolution;

    public ResolutionProposal(final IMarkerResolution resolution) {
        fResolution = resolution;
    }

    @Override
    public void apply(final IDocument document) {
        fResolution.run(null);
    }

    @Override
    public String getAdditionalProposalInfo() {
        if (fResolution instanceof IMarkerResolution2) {
            return ((IMarkerResolution2) fResolution).getDescription();
        }
        if (fResolution instanceof ICompletionProposal) {
            return ((ICompletionProposal) fResolution).getAdditionalProposalInfo();
        }
        return null;
    }

    @Override
    public IContextInformation getContextInformation() {
        return null;
    }

    @Override
    public String getDisplayString() {
        return fResolution.getLabel();
    }

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

    @Override
    public Point getSelection(final IDocument document) {
        if (fResolution instanceof ICompletionProposal) {
            return ((ICompletionProposal) fResolution).getSelection(document);
        }
        return null;
    }

}
