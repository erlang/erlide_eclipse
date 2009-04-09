/**
 * 
 */
package org.erlide.wrangler.refactoring.duplicatedcode.ui;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.erlide.wrangler.refactoring.duplicatedcode.Activator;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeElement;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeInstanceElement;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedFileElement;

class DuplicatesViewLabelProvider extends LabelProvider {
	Image erlangFileImage;
	Image codeSnippetImage;
	Image duplicateImage;

	public DuplicatesViewLabelProvider() {
		createImages();
	}

	@Override
	public void dispose() {
		erlangFileImage.dispose();
		codeSnippetImage.dispose();
		duplicateImage.dispose();
	}

	private void createImages() {
		erlangFileImage = Activator.getImageDescriptor("icons/erlFile.gif")
				.createImage();
		codeSnippetImage = Activator.getImageDescriptor("icons/match.gif")
				.createImage();
		duplicateImage = Activator.getImageDescriptor("icons/codeSnippet.gif")
				.createImage();
	}

	public String getText(Object obj) {
		return obj.toString();
	}

	public Image getImage(Object obj) {
		if (obj instanceof DuplicatedFileElement) {
			return erlangFileImage;
		} else if (obj instanceof DuplicatedCodeElement) {
			return duplicateImage;
		} else if (obj instanceof DuplicatedCodeInstanceElement)
			return codeSnippetImage;
		String imageKey = ISharedImages.IMG_OBJ_FILE;
		return PlatformUI.getWorkbench().getSharedImages().getImage(imageKey);
	}
}