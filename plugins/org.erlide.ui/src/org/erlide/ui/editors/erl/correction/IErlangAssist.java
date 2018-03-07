package org.erlide.ui.editors.erl.correction;

import java.util.Collection;

import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;

public interface IErlangAssist {

    boolean validAt(IQuickAssistInvocationContext invocationContext);

    Collection<? extends ICompletionProposal> getProposals();

}
