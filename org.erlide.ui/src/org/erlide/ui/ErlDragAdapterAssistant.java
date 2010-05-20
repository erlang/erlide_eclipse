package org.erlide.ui;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.ui.navigator.CommonDragAdapterAssistant;
import org.eclipse.ui.part.PluginTransfer;
import org.eclipse.ui.views.navigator.LocalSelectionTransfer;

public class ErlDragAdapterAssistant extends CommonDragAdapterAssistant {

	public ErlDragAdapterAssistant() {
	}

	@Override
	public Transfer[] getSupportedTransferTypes() {
		return new Transfer[] { PluginTransfer.getInstance(),
				TextTransfer.getInstance(),
				LocalSelectionTransfer.getInstance() };
	}

	@Override
	public boolean setDragData(final DragSourceEvent anEvent,
			final IStructuredSelection selection) {
		return false;
	}

}
