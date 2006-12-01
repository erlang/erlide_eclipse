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

import org.eclipse.jface.action.Action;

/**
 * A base class for evaluation state dependent actions.
 */
public abstract class ScratchAction extends Action implements
		IScratchStateChangedListener {

	private ScratchEditor fEditor;

	public ScratchAction(ScratchEditor editor) {
		setEditor(editor);
	}

	public void setEditor(ScratchEditor editor) {
		if (fEditor != null) {
			fEditor.removeScratchStateChangedListener(this);
		}
		fEditor = editor;

		if (fEditor != null) {
			if (fEditor.getFile() == null) { // external file
				setEnabled(false);
				return;
			}
			fEditor.addScratchStateChangedListener(this);
		}
		scratchStateChanged(fEditor);
	}

	protected ScratchEditor getEditor() {
		return fEditor;
	}
}
