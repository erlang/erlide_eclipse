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

    public ProcessCellModifier(final TableViewer tableViewer) {
        this.tableViewer = tableViewer;
    }

    @Override
    public boolean canModify(final Object element, final String property) {
        // when cell from processes column was clicked
        try {
            final ProcessColumn column = ProcessColumn.valueOf(property);
            // only column with checkbox can be modified
            if (!ProcessColumn.SELECTED.equals(column)) {
                return false;
            }
        } catch (final Exception e) {
        }

        // when cell from flag column was clicked
        return true;
    }

    @Override
    public Object getValue(final Object element, final String property) {
        final TracedProcess process = (TracedProcess) element;

        try {
            switch (ProcessColumn.valueOf(property)) {
            case INITIAL_CALL:
                return process.getInitialCall();
            case NAME:
                return process.getName();
            case SELECTED:
                return process.isSelected();
            default:
            }
        } catch (final Exception e) {
        }
        return process.hasFlag(ProcessFlag.valueOf(property));
    }

    @Override
    public void modify(final Object element, final String property,
            final Object value) {
        final TracedProcess process = (TracedProcess) ((TableItem) element)
                .getData();

        // processes column
        try {
            if (ProcessColumn.SELECTED.equals(ProcessColumn.valueOf(property))) {
                process.setSelected((Boolean) value);
                tableViewer.update(process, null);
            }
            return;
        } catch (final Exception e) {
        }

        // flag column
        final ProcessFlag flag = ProcessFlag.valueOf(property);
        if ((Boolean) value) {
            process.setFlag(flag);
        } else {
            process.unSetFlag(flag);
        }
        tableViewer.update(process, null);
    }
}
