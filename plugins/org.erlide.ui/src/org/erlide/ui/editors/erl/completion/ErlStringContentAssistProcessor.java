package org.erlide.ui.editors.erl.completion;

import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.eclipse.jface.text.source.ISourceViewer;
import org.erlide.engine.model.root.IErlModule;
import org.erlide.engine.model.root.IErlProject;

public class ErlStringContentAssistProcessor extends AbstractErlContentAssistProcessor {

    private static final char[] NONE = {};

    public ErlStringContentAssistProcessor(final ISourceViewer sourceViewer,
            final IErlModule module, final IErlProject project,
            final ContentAssistant contentAssistant) {
        super(sourceViewer, module, project, contentAssistant);
    }

    @Override
    protected boolean isInString() {
        return true;
    }

    @Override
    public IContextInformation[] computeContextInformation(final ITextViewer viewer,
            final int offset) {
        return null;
    }

    @Override
    public char[] getCompletionProposalAutoActivationCharacters() {
        return ErlStringContentAssistProcessor.NONE;
    }

    @Override
    public char[] getContextInformationAutoActivationCharacters() {
        return null;
    }

    @Override
    public String getErrorMessage() {
        return null;
    }

    @Override
    public IContextInformationValidator getContextInformationValidator() {
        return null;
    }

}
