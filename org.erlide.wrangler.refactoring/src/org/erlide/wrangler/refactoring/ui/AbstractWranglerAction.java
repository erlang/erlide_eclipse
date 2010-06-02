/*******************************************************************************
 * Copyright (c) 2010 György Orosz.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     György Orosz - initial API and implementation
 ******************************************************************************/
package org.erlide.wrangler.refactoring.ui;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

public abstract class AbstractWranglerAction implements IEditorActionDelegate,
		IWorkbenchWindowActionDelegate {

	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		// GlobalParameters.setEditor(targetEditor);

	}

	public void selectionChanged(IAction action, ISelection selection) {
		// GlobalParameters.setSelection(selection);

	}

	public void dispose() {
	}

	public void init(IWorkbenchWindow window) {
	}

}
