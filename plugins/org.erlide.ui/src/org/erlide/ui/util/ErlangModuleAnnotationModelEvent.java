package org.erlide.ui.util;

import org.eclipse.core.resources.IResource;
import org.eclipse.jface.text.source.AnnotationModelEvent;
import org.eclipse.jface.text.source.IAnnotationModel;

public class ErlangModuleAnnotationModelEvent extends AnnotationModelEvent {

    public ErlangModuleAnnotationModelEvent(final IAnnotationModel model) {
        super(model);
    }

    public boolean includesProblemMarkerAnnotationChanges() {
        return false;
    }

    public IResource getUnderlyingResource() {
        return null;
    }

}
