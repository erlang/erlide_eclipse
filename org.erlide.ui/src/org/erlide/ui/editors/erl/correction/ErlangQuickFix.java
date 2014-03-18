package org.erlide.ui.editors.erl.correction;

import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IMarkerResolution2;

public abstract class ErlangQuickFix implements IMarkerResolution2 {

    String label;
    private final String description;
    private final Image image;

    public ErlangQuickFix(final String label, final String description, final Image image) {
        this.label = label;
        this.description = description;
        this.image = image;
    }

    @Override
    public String getLabel() {
        return label;
    }

    @Override
    public void run(final IMarker marker) {
        MessageDialog.openInformation(null, "QuickFix Demo",
                "This quick-fix is not yet implemented");
    }

    @Override
    public String getDescription() {
        return description;
    }

    @Override
    public Image getImage() {
        return image;
    }
}
