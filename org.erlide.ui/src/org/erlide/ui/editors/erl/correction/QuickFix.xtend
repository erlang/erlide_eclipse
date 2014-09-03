package org.erlide.ui.editors.erl.correction

import java.util.List
import org.eclipse.core.resources.IMarker
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext
import org.eclipse.swt.graphics.Image
import org.eclipse.ui.IMarkerResolution2
import org.eclipse.xtend.lib.annotations.Accessors

@Accessors
class QuickFix implements IMarkerResolution2 {
    String label;
    String description;
    Image image;
    List<String> tags;
    QuickFixExecutor executor;
    List<String> args;

    new() {
    }

    new(QuickFix other) {
        label = other.getLabel();
        description = other.getDescription();
        image = other.getImage();
        tags = other.getTags();
        executor = other.getExecutor();
    }

    override void run(IMarker marker) {
        if (marker !== null && !marker.exists()) {
            return;
        }
        executor.run(marker, this);
    }

    def boolean appliesAt(IQuickAssistInvocationContext invocationContext) {
        return executor.appliesAt(invocationContext);
    }

}
