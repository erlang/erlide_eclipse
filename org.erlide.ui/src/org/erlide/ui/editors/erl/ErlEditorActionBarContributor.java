/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/

package org.erlide.ui.editors.erl;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorPart;
// import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.texteditor.BasicTextEditorActionContributor;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;
import org.erlide.ui.actions.GotoAnnotationAction;

public class ErlEditorActionBarContributor extends
		BasicTextEditorActionContributor {

	public ErlEditorActionBarContributor() {
		super();

		// contentAssistProposal = new RetargetTextEditorAction(ErlideUIMessages
		// .getResourceBundle(), "ContentAssistProposal.");
		// System.out.println("AC 1");

		fPreviousAnnotation = new GotoAnnotationAction(
				"PreviousAnnotation.", false); //$NON-NLS-1$
		fNextAnnotation = new GotoAnnotationAction("NextAnnotation.", true); //$NON-NLS-1$
	}

	// protected RetargetTextEditorAction contentAssistProposal;
	private GotoAnnotationAction fPreviousAnnotation;

	private GotoAnnotationAction fNextAnnotation;

	@Override
	public void contributeToMenu(IMenuManager menuManager) {
		// final IMenuManager editMenu = menuManager
		// .findMenuUsingPath(IWorkbenchActionConstants.M_EDIT);
		// if (editMenu != null)
		// {
		// editMenu.add(new Separator());
		// editMenu.add(contentAssistProposal);
		// }
	}

	@Override
	public void setActiveEditor(IEditorPart part) {
		super.setActiveEditor(part);

		ITextEditor editor = null;
		if (part instanceof ITextEditor) {
			editor = (ITextEditor) part;
		}

		// contentAssistProposal.setAction(getAction(editor,
		// "ContentAssistProposal"));

		// IActionBars bars = getActionBars();
		// bars.setGlobalActionHandler(ErlangActionIds.COMMENT,
		// getAction(editor,
		// "Comment"));
		// bars.setGlobalActionHandler(ErlangActionIds.UNCOMMENT,
		// getAction(editor,
		// "Uncomment"));

		fPreviousAnnotation.setEditor(editor);
		fNextAnnotation.setEditor(editor);
	}

	/*
	 * @see IEditorActionBarContributor#init(IActionBars, IWorkbenchPage)
	 */
	@Override
	public void init(IActionBars bars, IWorkbenchPage page) {
		super.init(bars, page);
		// register actions that have a dynamic editor.
		bars.setGlobalActionHandler(
				ITextEditorActionDefinitionIds.GOTO_NEXT_ANNOTATION,
				fNextAnnotation);
		bars.setGlobalActionHandler(
				ITextEditorActionDefinitionIds.GOTO_PREVIOUS_ANNOTATION,
				fPreviousAnnotation);
		bars
				.setGlobalActionHandler(ActionFactory.NEXT.getId(),
						fNextAnnotation);
		bars.setGlobalActionHandler(ActionFactory.PREVIOUS.getId(),
				fPreviousAnnotation);
	}

}
