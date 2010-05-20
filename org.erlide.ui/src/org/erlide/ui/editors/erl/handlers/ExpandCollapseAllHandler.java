package org.erlide.ui.editors.erl.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.handlers.HandlerUtil;
import org.erlide.ui.editors.erl.ErlangEditor;

public class ExpandCollapseAllHandler extends AbstractHandler implements
		IHandler {

	public Object execute(final ExecutionEvent event) throws ExecutionException {
		IEditorPart activeEditor = HandlerUtil.getActiveEditor(event);
		if (!(activeEditor instanceof ErlangEditor)) {
			return null;
		}
		ErlangEditor editor = (ErlangEditor) activeEditor;
		boolean collapse = event.getParameter(
				"org.erlide.ui.commands.expandCollapseParameter").equals(
				"collapse");
		boolean comments = event.getParameter(
				"org.erlide.ui.commands.foldWhatParameter").equals("comments");
		editor.expandCollapseFunctionsOrComments(collapse, comments);
		return null;
	}

	@Override
	public boolean isEnabled() {
		// TODO Auto-generated method stub
		return super.isEnabled();
	}
}
