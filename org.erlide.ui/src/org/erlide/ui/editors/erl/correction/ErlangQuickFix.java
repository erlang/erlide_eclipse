package org.erlide.ui.editors.erl.correction;

import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.IMarkerResolution;

public class ErlangQuickFix implements IMarkerResolution {

    String label;

    public ErlangQuickFix(final String label) {
        this.label = label;
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
}
