/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.erl.correction;

import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;
import org.eclipse.jface.text.quickassist.IQuickAssistProcessor;
import org.eclipse.jface.text.source.Annotation;
import org.erlide.ui.editors.erl.completion.NoCompletionsProposal;

public class ErlangQuickAssistProcessor implements IQuickAssistProcessor {

    @Override
    public boolean canAssist(
            final IQuickAssistInvocationContext invocationContext) {
        return false;
    }

    @Override
    public boolean canFix(final Annotation annotation) {
        return true;
    }

    @Override
    public ICompletionProposal[] computeQuickAssistProposals(
            final IQuickAssistInvocationContext invocationContext) {
        final ICompletionProposal[] result = new ICompletionProposal[] { new NoCompletionsProposal() };
        return result;
    }

    @Override
    public String getErrorMessage() {
        return "not implemented yet";
    }

}
