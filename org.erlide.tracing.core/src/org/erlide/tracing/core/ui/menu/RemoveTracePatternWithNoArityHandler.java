package org.erlide.tracing.core.ui.menu;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.ui.PlatformUI;
import org.erlide.core.internal.model.erlang.ErlFunction;
import org.erlide.tracing.core.TraceBackend;
import org.erlide.tracing.core.mvc.model.TracePattern;

public class RemoveTracePatternWithNoArityHandler extends AbstractHandler {

    @Override
    public Object execute(final ExecutionEvent event) throws ExecutionException {
        final ISelection selection = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage().getSelection();

        if (selection instanceof ITreeSelection) {

            final Object firstElement = ((ITreeSelection) selection)
                    .getFirstElement();

            if (firstElement instanceof ErlFunction) {
                final ErlFunction function = (ErlFunction) firstElement;
                final TracePattern tracePattern = new TracePattern(true);
                tracePattern.setFunctionName(function.getFunctionName());
                tracePattern
                        .setModuleName(function.getModule().getModuleName());
                TraceBackend.getInstance().removeTracePattern(tracePattern);
            }
        }
        return null;
    }
}
