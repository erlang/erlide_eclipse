package org.erlide.tracing.core.mvc.controller;

import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.widgets.TableItem;
import org.erlide.tracing.core.ProcessFlag;
import org.erlide.tracing.core.mvc.model.TracedProcess;
import org.erlide.tracing.core.mvc.view.ProcessColumn;

/**
 * Cell modifier for processes table.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class ProcessCellModifier implements ICellModifier {

    private final TableViewer tableViewer;

    public ProcessCellModifier(TableViewer tableViewer) {
        this.tableViewer = tableViewer;
    }

    public boolean canModify(Object element, String property) {
        // when cell from processes column was clicked
        try {
            ProcessColumn column = ProcessColumn.valueOf(property);
            // only column with checkbox can be modified
            if (!ProcessColumn.SELECTED.equals(column))
                return false;
        } catch (Exception e) {
        }

        // when cell from flag column was clicked
        return true;
    }

    public Object getValue(Object element, String property) {
        TracedProcess process = (TracedProcess) element;

        try {
            switch (ProcessColumn.valueOf(property)) {
            case INITIAL_CALL:
                return process.getInitialCall();
            case NAME:
                return process.getName();
            case SELECTED:
                return process.isSelected();
            }
        } catch (Exception e) {
        }
        return process.hasFlag(ProcessFlag.valueOf(property));
    }

    public void modify(Object element, String property, Object value) {
        TracedProcess process = (TracedProcess) ((TableItem) element).getData();

        // processes column
        try {
            if (ProcessColumn.SELECTED.equals(ProcessColumn.valueOf(property))) {
                process.setSelected((Boolean) value);
                tableViewer.update(process, null);
            }
            return;
        } catch (Exception e) {
        }

        // flag column
        ProcessFlag flag = ProcessFlag.valueOf(property);
        if ((Boolean) value)
            process.setFlag(flag);
        else
            process.unSetFlag(flag);
        tableViewer.update(process, null);
    }
}
