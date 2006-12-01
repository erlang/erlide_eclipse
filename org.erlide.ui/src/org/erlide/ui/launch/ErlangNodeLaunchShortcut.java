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
package org.erlide.ui.launch;

import org.eclipse.debug.ui.ILaunchShortcut;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;

public class ErlangNodeLaunchShortcut implements ILaunchShortcut {

	public void launch(ISelection selection, String mode) {
		// TODO Auto-generated method stub
		System.out.println("** Launch:: " + selection.toString());

	}

	public void launch(IEditorPart editor, String mode) {
		// TODO Auto-generated method stub
		System.out.println("** Launch :: " + editor.getTitle());
	}

}
