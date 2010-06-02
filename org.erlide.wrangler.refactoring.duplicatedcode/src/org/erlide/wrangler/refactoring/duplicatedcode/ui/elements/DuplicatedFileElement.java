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
package org.erlide.wrangler.refactoring.duplicatedcode.ui.elements;

import org.eclipse.core.resources.IFile;

public class DuplicatedFileElement extends AbstractResultTreeParent {
	IFile containingFile;

	// ITextEditor editor;

	public DuplicatedFileElement(IFile containFile) {
		// this.editor = editor;
		this.containingFile = containFile;
	}

	@Override
	public String getName() {
		return containingFile.getName();
	}

	/*
	 * public ITextEditor getTextEditor() { return editor; }
	 */

	public IFile getContainingFile() {
		return containingFile;
	}

}
