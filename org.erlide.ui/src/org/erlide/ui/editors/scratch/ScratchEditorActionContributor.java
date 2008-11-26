/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.editors.scratch;

import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.texteditor.BasicTextEditorActionContributor;
import org.erlide.ui.ErlideUIConstants;

/**
 * Contributions of the Erlang Scratch Editor to the Workbench's tool and menu
 * bar.
 */
public class ScratchEditorActionContributor extends
		BasicTextEditorActionContributor {

	protected ScratchEditor fScratchEditor;

	private StopAction fStopAction;

	private EvaluateAction fEvaluateAction;

	public ScratchEditorActionContributor() {
		super();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.part.EditorActionBarContributor#contributeToToolBar(org.eclipse.jface.action.IToolBarManager)
	 */
	@Override
	public void contributeToToolBar(IToolBarManager toolBarManager) {

		if (fStopAction == null || fEvaluateAction == null) {
			toolBarManager.add(new Separator(
					ErlideUIConstants.EVALUATION_GROUP));
			return;
		}
		toolBarManager.add(fEvaluateAction);
		toolBarManager.add(fStopAction);
		toolBarManager.update(false);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IEditorActionBarContributor#setActiveEditor(org.eclipse.ui.IEditorPart)
	 */
	@Override
	public void setActiveEditor(IEditorPart part) {

		super.setActiveEditor(part);
		fScratchEditor = null;
		if (part instanceof ScratchEditor) {
			fScratchEditor = (ScratchEditor) part;
		}

		updateStatus(fScratchEditor);
	}

	protected void initializeActions() {
		fEvaluateAction = new EvaluateAction(fScratchEditor);
		fStopAction = new StopAction(fScratchEditor);
	}

	protected void updateStatus(ScratchEditor editor) {
		String message = ""; //$NON-NLS-1$
		if (editor != null && editor.isEvaluating()) {
			message = ScratchMessages
					.getString("ScratchActionContributor.evalMsg"); //$NON-NLS-1$
		}
		getActionBars().getStatusLineManager().setMessage(message);
	}
}