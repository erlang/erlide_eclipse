package org.ttb.integration.mvc.controller;

import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.widgets.TableItem;
import org.ttb.integration.ProcessFlag;
import org.ttb.integration.mvc.model.ProcessOnList;
import org.ttb.integration.mvc.view.ProcessColumn;

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

    @Override
    public boolean canModify(Object element, String property) {
        try {
            ProcessColumn column = ProcessColumn.valueOf(property);
            // only column with checkboxes can be modified
            if (!ProcessColumn.SELECTED.equals(column))
                return false;
        } catch (Exception e) {
        }

        return true;
    }

    @Override
    public Object getValue(Object element, String property) {
        ProcessOnList process = (ProcessOnList) element;

        try {
            switch (ProcessColumn.valueOf(property)) {
            case INITIAL_CALL:
                return process.getInitialCall();
            case NAME:
                return process.getName();
                // case PID:
                // return process.getPid().toString();
            case SELECTED:
                return process.isSelected();
            }
        } catch (Exception e) {
        }
        return process.hasFlag(ProcessFlag.valueOf(property));
    }

    @Override
    public void modify(Object element, String property, Object value) {
        ProcessOnList process = (ProcessOnList) ((TableItem) element).getData();

        try {
            if (ProcessColumn.SELECTED.equals(ProcessColumn.valueOf(property))) {
                process.setSelected((Boolean) value);
                tableViewer.update(process, null);
            }
            return;
        } catch (Exception e) {
        }
        ProcessFlag flag = ProcessFlag.valueOf(property);
        if ((Boolean) value)
            process.setFlag(flag);
        else
            process.unSetFlag(flag);
        tableViewer.update(process, null);
    }
}
