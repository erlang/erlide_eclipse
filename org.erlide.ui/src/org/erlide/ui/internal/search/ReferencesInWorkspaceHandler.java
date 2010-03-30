package org.erlide.ui.internal.search;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.handlers.HandlerUtil;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.search.ErlangSearchPattern;

public class ReferencesInWorkspaceHandler extends AbstractHandler {

	public ReferencesInWorkspaceHandler() {
		super();
	}

	public Object execute(final ExecutionEvent event) throws ExecutionException {
		Object selection = HandlerUtil.getVariable(event,
				ISources.ACTIVE_CURRENT_SELECTION_NAME);
		Shell shell = (Shell) HandlerUtil.getVariable(event,
				ISources.ACTIVE_SHELL_NAME);
		if (selection instanceof IStructuredSelection) {
			Object firstElement = ((IStructuredSelection) selection)
					.getFirstElement();
			if (firstElement instanceof IErlElement) {
				IErlElement element = (IErlElement) firstElement;
				final ErlangSearchPattern pattern = SearchUtil
						.getSearchPatternFromErlElementAndLimitTo(element,
								ErlangSearchPattern.REFERENCES);
				SearchUtil.runQuery(pattern, SearchUtil.getWorkspaceScope(),
						SearchUtil.getWorkspaceScopeDescription(), shell);
			}
		}
		return null;
	}
}