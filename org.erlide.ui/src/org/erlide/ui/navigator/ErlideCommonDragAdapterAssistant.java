package org.erlide.ui.navigator;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.ui.navigator.CommonDragAdapterAssistant;

public class ErlideCommonDragAdapterAssistant extends
        CommonDragAdapterAssistant {

    public ErlideCommonDragAdapterAssistant() {
        // TODO Auto-generated constructor stub
    }

    @Override
    public Transfer[] getSupportedTransferTypes() {
        // TODO Auto-generated method stub
        return new Transfer[0];
    }

    @Override
    public boolean setDragData(final DragSourceEvent anEvent,
            final IStructuredSelection selection) {
        // TODO Auto-generated method stub
        return false;
    }

}
