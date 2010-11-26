package org.erlide.ui;

import org.eclipse.jface.util.LocalSelectionTransfer;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.ui.navigator.CommonDragAdapterAssistant;
import org.eclipse.ui.part.PluginTransfer;

public class ErlDragAdapterAssistant extends CommonDragAdapterAssistant {

    public ErlDragAdapterAssistant() {
    }

    @Override
    public Transfer[] getSupportedTransferTypes() {
        return new Transfer[] { PluginTransfer.getInstance(),
                TextTransfer.getInstance(),
                LocalSelectionTransfer.getTransfer() };
    }

    @Override
    public boolean setDragData(final DragSourceEvent anEvent,
            final IStructuredSelection selection) {
        return false;
    }

}
