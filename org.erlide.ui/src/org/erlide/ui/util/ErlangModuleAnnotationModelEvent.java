package org.erlide.ui.util;

import org.eclipse.core.resources.IResource;
import org.eclipse.jface.text.source.AnnotationModelEvent;
import org.eclipse.jface.text.source.IAnnotationModel;

public class ErlangModuleAnnotationModelEvent extends AnnotationModelEvent {

	public ErlangModuleAnnotationModelEvent(final IAnnotationModel model) {
		super(model);
		// TODO Auto-generated constructor stub
	}

	public boolean includesProblemMarkerAnnotationChanges() {
		// TODO Auto-generated method stub
		return false;
	}

	public IResource getUnderlyingResource() {
		// TODO Auto-generated method stub
		return null;
	}

}
