package org.erlide.ui.editors.erl.correction

import java.util.List
import org.eclipse.core.resources.IMarker
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext
import org.eclipse.swt.graphics.Image
import org.eclipse.ui.IMarkerResolution2

class QuickFix implements IMarkerResolution2 {
    @Property String label;
    @Property String description;
    @Property Image image;
    @Property List<String> tags;
    @Property QuickFixExecutor executor;
    @Property List<String> args;

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
