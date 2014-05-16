package org.erlide.ui.editors.erl.correction.assists;

import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;
import org.erlide.ui.editors.erl.correction.QuickFixExecutor;

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
