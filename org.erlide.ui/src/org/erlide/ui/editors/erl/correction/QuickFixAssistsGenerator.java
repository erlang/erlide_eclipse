package org.erlide.ui.editors.erl.correction;

import java.util.Collection;
import java.util.List;

import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;
import org.erlide.util.services.ExtensionUtils;

import com.google.common.collect.Lists;

public class QuickFixAssistsGenerator {

    private final List<IErlangAssist> extensions;

    public QuickFixAssistsGenerator() {
        extensions = ExtensionUtils.getExtensions("org.erlide.ui.quickfix",
                IErlangAssist.class);
    }

    public Collection<ICompletionProposal> getAssists(
            final IQuickAssistInvocationContext invocationContext) {

        System.out.println("ASSIST " + invocationContext.getOffset());
        System.out.println("  " + extensions.size() + " " + extensions);

        final List<ICompletionProposal> result = Lists.newArrayList();

        for (final IErlangAssist assist : extensions) {
            if (assist.validAt(invocationContext)) {
                result.addAll(assist.getProposals());
            }
        }

        return result;
    }

    public boolean hasAssists(final IQuickAssistInvocationContext invocationContext) {
        return true;
    }

}
