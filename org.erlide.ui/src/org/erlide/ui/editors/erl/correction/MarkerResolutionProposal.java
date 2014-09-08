package org.erlide.ui.editors.erl.correction;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.IMarkerResolution2;

public class MarkerResolutionProposal extends ResolutionProposal {

    private final IMarker fMarker;

    public MarkerResolutionProposal(final IMarkerResolution resolution,
            final IMarker marker) {
        super(resolution);
        fMarker = marker;
    }

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
        final String info = super.getAdditionalProposalInfo();
        if (info != null) {
            return info;
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
