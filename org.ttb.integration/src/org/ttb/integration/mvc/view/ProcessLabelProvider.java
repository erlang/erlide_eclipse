package org.ttb.integration.mvc.view;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.ttb.integration.Activator;
import org.ttb.integration.Images;
import org.ttb.integration.ProcessFlag;
import org.ttb.integration.mvc.model.ProcessOnList;

public class ProcessLabelProvider extends LabelProvider implements ITableLabelProvider {

    @Override
    public Image getColumnImage(Object element, int columnIndex) {
        ProcessOnList process = (ProcessOnList) element;

        if (ProcessColumn.SELECTED.ordinal() == columnIndex) {
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

    @Override
    public String getColumnText(Object element, int columnIndex) {

        ProcessOnList process = (ProcessOnList) element;
        if (columnIndex < ProcessColumn.values().length) {
            switch (ProcessColumn.getByIndex(columnIndex)) {
            case SELECTED:
                break;
            case NAME:
                return process.getName();
                // case PID:
                // return process.getPid().node() + ": " +
                // process.getPid().serial();
            case INITIAL_CALL:
                return process.getInitialCall();
            }
        }
        return "";
    }
}
