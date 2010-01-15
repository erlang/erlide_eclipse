package org.erlide.ui.wizards;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ICheckStateProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;

public class DirectoryTreeGroup extends Composite {

	// private final Label title;
	private final CheckboxTableViewer checkboxTableViewer;
	private final Set<String> checkedDirs;
	private List<String> allDirs;

	private class ContentProvider implements IStructuredContentProvider {

		public Object[] getElements(final Object inputElement) {
			return allDirs.toArray(new Object[allDirs.size()]);
		}

		public void dispose() {
			// do nothing
		}

		public void inputChanged(final Viewer viewer, final Object oldInput,
				final Object newInput) {
			// do nothing
		}

	}

	public DirectoryTreeGroup(final String title, final Composite parent) {
		super(parent, SWT.NONE);
		setLayout(new GridLayout());
		GridData layoutData = new GridData(GridData.VERTICAL_ALIGN_FILL
				| GridData.HORIZONTAL_ALIGN_FILL);
		setLayoutData(layoutData);

		// this.title = new Label(this, SWT.NONE);
		// this.title.setText(title);
		final Group group = new Group(parent, SWT.NONE);
		group.setLayout(new GridLayout());
		layoutData = new GridData(GridData.VERTICAL_ALIGN_FILL
				| GridData.HORIZONTAL_ALIGN_FILL);
		group.setLayoutData(layoutData);
		group.setText(title);

		checkboxTableViewer = CheckboxTableViewer.newCheckList(group, SWT.NONE);
		layoutData = new GridData(GridData.VERTICAL_ALIGN_FILL
				| GridData.HORIZONTAL_ALIGN_FILL);
		layoutData = new GridData(GridData.VERTICAL_ALIGN_FILL
				| GridData.HORIZONTAL_ALIGN_FILL);
		layoutData.heightHint = 90;
		layoutData.widthHint = 180;
		checkboxTableViewer.getControl().setLayoutData(layoutData);
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
		checkboxTableViewer.setCheckStateProvider(new ICheckStateProvider() {

			public boolean isGrayed(final Object element) {
				return false;
			}

			public boolean isChecked(final Object element) {
				return checkedDirs.contains(element);
			}
		});
		checkboxTableViewer.addCheckStateListener(new ICheckStateListener() {

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
		checkboxTableViewer.refresh();
	}

	public void setCheckedDirs(final List<String> checkedDirs) {
		this.checkedDirs.clear();
		for (final String i : checkedDirs) {
			this.checkedDirs.add(i);
		}
		checkboxTableViewer.refresh();
	}
}
