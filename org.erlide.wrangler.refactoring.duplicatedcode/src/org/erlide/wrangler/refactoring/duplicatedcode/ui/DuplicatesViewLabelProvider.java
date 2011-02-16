/**
 * 
 */
package org.erlide.wrangler.refactoring.duplicatedcode.ui;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.erlide.wrangler.refactoring.Activator;
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

        erlangFileImage = getImageDescriptor("icons/erlFile.gif").createImage();
        codeSnippetImage = getImageDescriptor("icons/match.gif").createImage();
        duplicateImage = getImageDescriptor("icons/codeSnippet.gif")
                .createImage();
    }

    @Override
    public String getText(final Object obj) {
        return obj.toString();
    }

    @Override
    public Image getImage(final Object obj) {
        if (obj instanceof DuplicatedFileElement) {
            return erlangFileImage;
        } else if (obj instanceof DuplicatedCodeElement) {
            return duplicateImage;
        } else if (obj instanceof DuplicatedCodeInstanceElement) {
            return codeSnippetImage;
        }
        final String imageKey = ISharedImages.IMG_OBJ_FILE;
        return PlatformUI.getWorkbench().getSharedImages().getImage(imageKey);
    }

    public static ImageDescriptor getImageDescriptor(final String name) {

        final ImageDescriptor descriptor = AbstractUIPlugin
                .imageDescriptorFromPlugin(Activator.PLUGIN_ID, name);

        return descriptor;
    }
}
