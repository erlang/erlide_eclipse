package org.ttb.integration.mvc.view;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.ttb.integration.Activator;
import org.ttb.integration.Images;
import org.ttb.integration.ProcessFlag;
import org.ttb.integration.mvc.model.TracedProcess;

public class ProcessLabelProvider extends LabelProvider implements ITableLabelProvider {

    public Image getColumnImage(Object element, int columnIndex) {
        TracedProcess process = (TracedProcess) element;

        if (ProcessColumn.SELECTED.ordinal() == columnIndex) {
            // process columns
            if (process.isSelected())
                return Activator.getDefault().getImageRegistry().get(Images.CHECKED.toString());
            else
                return Activator.getDefault().getImageRegistry().get(Images.UNCHECKED.toString());
        } else {
            // flag columns
            ProcessFlag flag = ProcessFlag.getByIndex(columnIndex - ProcessColumn.values().length);
            if (flag != null) {
                if (process.hasFlag(flag))
                    return Activator.getDefault().getImageRegistry().get(Images.CHECKED.toString());
                else
                    return Activator.getDefault().getImageRegistry().get(Images.UNCHECKED.toString());
            }
        }

        return null;
    }

    public String getColumnText(Object element, int columnIndex) {

        TracedProcess process = (TracedProcess) element;
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
