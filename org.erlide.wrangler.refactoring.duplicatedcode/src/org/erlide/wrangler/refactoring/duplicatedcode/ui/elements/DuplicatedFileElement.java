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
