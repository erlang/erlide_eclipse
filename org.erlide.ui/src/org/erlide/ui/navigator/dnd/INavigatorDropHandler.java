package org.erlide.ui.navigator.dnd;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.ui.navigator.CommonDropAdapter;

/**
 * Interface for implementing erlDndAdapter extension point
 * 
 * @author György Orosz
 * 
 */
public interface INavigatorDropHandler {
    /**
     * Function for handling drop event
     * 
     * @param dropAdapter
     * @param dropTargetEvent
     * @param target
     * @return
     */
    public IStatus handleDrop(CommonDropAdapter dropAdapter,
            DropTargetEvent dropTargetEvent, Object target);

    /**
     * Function for validating drop event
     * 
     * @param target
     * @param operation
     * @param transferType
     * @return
     */
    public IStatus validateDrop(Object target, int operation,
            TransferData transferType);

    /*
     * public boolean setDragData(final DragSourceEvent anEvent, final
     * IStructuredSelection selection);
     */

}
