package org.erlide.ui.wizards;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

public class DirectoryTreeGroup extends Composite {

    // private final Label title;
    private final CheckboxTableViewer checkboxTableViewer;
    private final Set<String> checkedDirs;
    private List<String> allDirs;

    private class ContentProvider implements IStructuredContentProvider {

        @Override
        public Object[] getElements(final Object inputElement) {
            return allDirs.toArray(new Object[allDirs.size()]);
        }

        @Override
        public void dispose() {
            // do nothing
        }

        @Override
        public void inputChanged(final Viewer viewer, final Object oldInput,
                final Object newInput) {
            // do nothing
        }

    }

    public DirectoryTreeGroup(final Composite parent, final String title) {
        super(parent, SWT.NONE);
        setLayout(new GridLayout());
        GridData layoutData = new GridData(GridData.FILL_BOTH);
        setLayoutData(layoutData);

        // final Group group = new Group(parent, SWT.NONE);
        // group.setLayout(new GridLayout());
        // layoutData = new GridData(GridData.FILL_BOTH);
        // group.setLayoutData(layoutData);
        // group.setText(title);
        final Label label = new Label(this, SWT.NONE);
        label.setText(title);
        checkboxTableViewer = CheckboxTableViewer
                .newCheckList(this, SWT.BORDER);
        layoutData = new GridData(GridData.FILL_BOTH);
        layoutData.heightHint = 90;
        layoutData.widthHint = 180;
        checkboxTableViewer.getTable().setLayoutData(layoutData);
        checkboxTableViewer.setLabelProvider(new LabelProvider() {

            @Override
            public String getText(final Object element) {
                if (element instanceof String) {
                    return (String) element;
                }
                return null;
            }

        });
        checkboxTableViewer.setContentProvider(new ContentProvider());
        checkboxTableViewer.addCheckStateListener(new ICheckStateListener() {

            @Override
            public void checkStateChanged(final CheckStateChangedEvent event) {
                checkedDirs.clear();
                for (final Object o : checkboxTableViewer.getCheckedElements()) {
                    checkedDirs.add((String) o);
                }
            }
        });
        allDirs = new ArrayList<String>();
        checkedDirs = new HashSet<String>();
        checkboxTableViewer.setInput(allDirs);
        // final GridData data = new GridData(GridData.FILL_BOTH);
        // data.horizontalSpan = 2;
        // data.widthHint = IDialogConstants.ENTRY_FIELD_WIDTH;
        // data.heightHint = IDialogConstants.ENTRY_FIELD_WIDTH;
        // checkboxTableViewer.getTable().setLayoutData(data);
    }

    public void refresh() {
        checkboxTableViewer.refresh();
    }

    public void setAllDirs(final List<String> allDirs) {
        this.allDirs = allDirs;
        checkboxTableViewer.setInput(allDirs);
        setChecked();
        checkboxTableViewer.refresh();
    }

    public void setCheckedDirs(final List<String> checkedDirs) {
        this.checkedDirs.clear();
        for (final String i : checkedDirs) {
            this.checkedDirs.add(i);
        }
        setChecked();
        checkboxTableViewer.refresh();
    }

    private void setChecked() {
        for (final String i : allDirs) {
            checkboxTableViewer.setChecked(i, checkedDirs.contains(i));
        }
    }

    public Collection<String> getChecked() {
        return checkedDirs;
    }
}
