package org.erlide.ui.editors.erl.correction;

import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;

public class ConvertMultiStringQuickFix extends QuickFixExecutor {

    @Override
    public void run() throws Exception {
        // TODO Auto-generated method stub
        super.run();
    }

    @Override
    public boolean appliesAt(final IQuickAssistInvocationContext invocationContext) {
        // final int offset = invocationContext.getOffset();
        // final ISourceViewer viewer = invocationContext.getSourceViewer();

        return false;
    }

}
