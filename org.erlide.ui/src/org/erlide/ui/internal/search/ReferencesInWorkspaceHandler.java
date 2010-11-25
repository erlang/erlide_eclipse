package org.erlide.ui.internal.search;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.handlers.HandlerUtil;
import org.erlide.core.erlang.IErlElement;

import erlang.ErlangSearchPattern;
import erlang.ErlangSearchPattern.LimitTo;

public class ReferencesInWorkspaceHandler extends AbstractHandler {

    public ReferencesInWorkspaceHandler() {
        super();
    }

    public Object execute(final ExecutionEvent event) throws ExecutionException {
        final Object selection = HandlerUtil.getVariable(event,
                ISources.ACTIVE_CURRENT_SELECTION_NAME);
        final Shell shell = (Shell) HandlerUtil.getVariable(event,
                ISources.ACTIVE_SHELL_NAME);
        if (selection instanceof IStructuredSelection) {
            final Object firstElement = ((IStructuredSelection) selection)
                    .getFirstElement();
            if (firstElement instanceof IErlElement) {
                final IErlElement element = (IErlElement) firstElement;
                final ErlangSearchPattern pattern = ErlangSearchPattern
                        .getSearchPatternFromErlElementAndLimitTo(element,
                                LimitTo.REFERENCES);
                SearchUtil.runQuery(pattern, SearchUtil.getWorkspaceScope(),
                        SearchUtil.getWorkspaceExternalScope(),
                        SearchUtil.getWorkspaceScopeDescription(), shell);
            }
        }
        return null;
    }
}