package org.erlide.tracing.core.mvc.view;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.erlide.tracing.core.Activator;
import org.erlide.tracing.core.Images;
import org.erlide.tracing.core.ProcessFlag;
import org.erlide.tracing.core.mvc.model.TracedProcess;

public class ProcessLabelProvider extends LabelProvider implements
        ITableLabelProvider {

    @Override
    public Image getColumnImage(final Object element, final int columnIndex) {
        final TracedProcess process = (TracedProcess) element;

        if (ProcessColumn.SELECTED.ordinal() == columnIndex) {
            // process columns
            if (process.isSelected()) {
                return Activator.getDefault().getImageRegistry()
                        .get(Images.CHECKED.toString());
            } else {
                return Activator.getDefault().getImageRegistry()
                        .get(Images.UNCHECKED.toString());
            }
        } else {
            // flag columns
            final ProcessFlag flag = ProcessFlag.getByIndex(columnIndex
                    - ProcessColumn.values().length);
            if (flag != null) {
                if (process.hasFlag(flag)) {
                    return Activator.getDefault().getImageRegistry()
                            .get(Images.CHECKED.toString());
                } else {
                    return Activator.getDefault().getImageRegistry()
                            .get(Images.UNCHECKED.toString());
                }
            }
        }

        return null;
    }

    @Override
    public String getColumnText(final Object element, final int columnIndex) {

        final TracedProcess process = (TracedProcess) element;
        if (columnIndex < ProcessColumn.values().length) {
            switch (ProcessColumn.getByIndex(columnIndex)) {
            case SELECTED:
                break;
            case NAME:
                return process.getName();
            case NODE:
                return process.getNode();
            case INITIAL_CALL:
                return process.getInitialCall();
            }
        }
        return "";
    }
}
