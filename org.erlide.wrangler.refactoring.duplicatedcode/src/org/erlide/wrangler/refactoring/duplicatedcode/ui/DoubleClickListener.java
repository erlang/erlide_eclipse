/**
 * 
 */
package org.erlide.wrangler.refactoring.duplicatedcode.ui;

import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeElement;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeInstanceElement;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedFileElement;
import org.erlide.wrangler.refactoring.util.WranglerUtils;

class DoubleClickListener implements IDoubleClickListener {

	DoubleClickListener() {

	}

	public void doubleClick(DoubleClickEvent event) {
		ISelection selection = event.getSelection();

		Object obj = ((IStructuredSelection) selection).getFirstElement();

		if (obj instanceof DuplicatedCodeInstanceElement) {
			higlightCodePart((DuplicatedCodeInstanceElement) obj);
		} else if (obj instanceof DuplicatedCodeElement) {
			higlightCodePart(((DuplicatedCodeElement) obj).getCodePart());
		} else if (obj instanceof DuplicatedFileElement) {
			DuplicatedFileElement obj2 = (DuplicatedFileElement) obj;
			WranglerUtils.openFile(obj2.getContainingFile());
		}

	}

	public void higlightCodePart(DuplicatedCodeInstanceElement codePart) {
		ITextEditor textEditor = (ITextEditor) WranglerUtils.openFile(codePart
				.getContainingFile());
		WranglerUtils.highlightOffsetSelection(codePart.getStartOffset(), codePart
				.getEndOffset(), textEditor);
	}
}