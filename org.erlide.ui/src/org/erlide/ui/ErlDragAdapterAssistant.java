package org.erlide.ui;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.ui.navigator.CommonDragAdapterAssistant;

public class ErlDragAdapterAssistant extends CommonDragAdapterAssistant {

	public ErlDragAdapterAssistant() {
	}

	@Override
	public Transfer[] getSupportedTransferTypes() {
		return new Transfer[] {};
	}

	@Override
	public boolean setDragData(final DragSourceEvent anEvent,
			final IStructuredSelection selection) {
		return false;
	}

}
