package org.erlide.ui.editors.erl.completion;

import java.util.EnumSet;

import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.eclipse.jface.text.source.ISourceViewer;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlProject;

public class ErlStringContentAssistProcessor extends AbstractErlContentAssistProcessor {

    private static final char[] NONE = new char[] {};

    public ErlStringContentAssistProcessor(final ISourceViewer sourceViewer,
            final IErlModule module, final IErlProject project,
            final ContentAssistant contentAssistant) {
        super(sourceViewer, module, project, contentAssistant);
    }

    @Override
    public IContextInformation[] computeContextInformation(final ITextViewer viewer,
            final int offset) {
        return null;
    }

    @Override
    public char[] getCompletionProposalAutoActivationCharacters() {
        return NONE;
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

    @Override
    protected EnumSet<Kinds> filterFlags(final EnumSet<Kinds> flags) {
        flags.retainAll(EnumSet.of(Kinds.INCLUDE_LIBS, Kinds.INCLUDES));
        return flags;
    }

    @Override
    protected String quoted(final String string, final Kinds kind) {
        return string;
    }

}
