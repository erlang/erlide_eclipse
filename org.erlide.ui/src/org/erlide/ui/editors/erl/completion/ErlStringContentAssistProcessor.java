package org.erlide.ui.editors.erl.completion;

import java.util.EnumSet;
import java.util.Set;

import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.eclipse.jface.text.source.ISourceViewer;
import org.erlide.core.model.erlang.IErlModule;

public class ErlStringContentAssistProcessor extends
        AbstractErlContentAssistProcessor implements IContentAssistProcessor {

    private static final char[] NONE = new char[] {};

    public ErlStringContentAssistProcessor(final ISourceViewer sourceViewer,
            final IErlModule module, final ContentAssistant contentAssistant) {
        super(sourceViewer, module, contentAssistant);
    }

    @Override
    public IContextInformation[] computeContextInformation(
            final ITextViewer viewer, final int offset) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public char[] getCompletionProposalAutoActivationCharacters() {
        return NONE;
    }

    @Override
    public char[] getContextInformationAutoActivationCharacters() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getErrorMessage() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public IContextInformationValidator getContextInformationValidator() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    protected Set<Kinds> filterFlags(final Set<Kinds> flags) {
        flags.retainAll(EnumSet.of(Kinds.INCLUDE_LIBS, Kinds.INCLUDES));
        return flags;
    }

    @Override
    protected String quoted(final String string, final Kinds kind) {
        return string;
    }

}
