/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.prefs;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableLayout;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.erlide.runtime.backend.InfoElement;
import org.erlide.runtime.backend.InstallationInfo;
import org.erlide.runtime.backend.InstallationInfoManager;
import org.erlide.runtime.backend.RuntimeInfo;
import org.erlide.runtime.backend.RuntimeInfoManager;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.util.SWTUtil;

/**
 * A preference page that displays installed installations in a table.
 * Installations can be added, removed, edited, and searched for.
 * <p>
 * It implements ISelectionProvider - it sends selection change events when the
 * checked runtime in the table changes, or when the "use default" button check
 * state changes.
 * </p>
 */
public class InstallationsPreferencePage extends PreferencePage implements
		IAddDialogRequestor<InstallationInfo>, ISelectionProvider,
		IWorkbenchPreferencePage {

	private Composite buttons;
	private Label tableLabel;
	private static final String INSTALLATIONS_PREFERENCE_PAGE = "INSTALLATIONS_PREFERENCE_PAGE";

	Collection<InstallationInfo> installations;
	InstallationInfo defaultInstallation;
	private boolean modified;

	/**
	 * The main list control
	 */
	protected CheckboxTableViewer fInstallationList;

	// Action buttons
	private Button fAddButton;
	private Button fRemoveButton;
	private Button fEditButton;
	private Button fSearchButton;

	// column weights
	protected float fWeight1 = 1 / 3F;
	protected float fWeight2 = 4 / 9F;

	// ignore column re-sizing when the table is being resized
	protected boolean fResizingTable = false;

	// index of column used for sorting
	private int fSortColumn = 0;

	/**
	 * Selection listeners (checked Runtime changes)
	 */
	private final ListenerList fSelectionListeners = new ListenerList();

	/**
	 * Previous selection
	 */
	private ISelection fPrevSelection = new StructuredSelection();

	/**
	 * Content provider to show a list of installations
	 */
	class InstallationContentProvider implements IStructuredContentProvider {

		public Object[] getElements(Object input) {
			return installations.toArray(new InstallationInfo[installations
					.size()]);
		}

		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		}

		public void dispose() {
		}

	}

	/**
	 * Label provider for installed runtimes table.
	 */
	static class InstallationLabelProvider extends LabelProvider implements
			ITableLabelProvider {

		/**
		 * @see ITableLabelProvider#getColumnText(Object, int)
		 */
		public String getColumnText(Object element, int columnIndex) {
			if (element instanceof InstallationInfo) {
				final InstallationInfo vm = (InstallationInfo) element;
				switch (columnIndex) {
				case 0:
					return vm.getName();
				case 1:
					return vm.getOtpHome();
				case 2:
					return vm.getVersion();
				}
			}
			return element.toString();
		}

		/**
		 * @see ITableLabelProvider#getColumnImage(Object, int)
		 */
		public Image getColumnImage(Object element, int columnIndex) {
			return null;
		}

	}

	public InstallationsPreferencePage() {
		super();
		setTitle(InstallationPreferenceMessages.Page_title);
		setDescription(InstallationPreferenceMessages.Page_description);

		refreshData();
	}

	private void refreshData() {
		installations = InstallationInfoManager.getDefault().getElements();
		defaultInstallation = InstallationInfoManager.getDefault()
				.getDefaultInstallation();
		modified = false;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener
	 * (org.eclipse.jface.viewers.ISelectionChangedListener)
	 */
	public void addSelectionChangedListener(ISelectionChangedListener listener) {
		fSelectionListeners.add(listener);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
	 */
	public ISelection getSelection() {
		return new StructuredSelection(fInstallationList.getCheckedElements());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener
	 * (org.eclipse.jface.viewers.ISelectionChangedListener)
	 */
	public void removeSelectionChangedListener(
			ISelectionChangedListener listener) {
		fSelectionListeners.remove(listener);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse
	 * .jface.viewers.ISelection)
	 */
	public void setSelection(ISelection selection) {
		if (selection instanceof IStructuredSelection) {
			if (!selection.equals(fPrevSelection)) {
				fPrevSelection = selection;
				final Object vm = ((IStructuredSelection) selection)
						.getFirstElement();
				if (vm == null) {
					fInstallationList.setCheckedElements(new Object[0]);
				} else {
					fInstallationList.setCheckedElements(new Object[] { vm });
					fInstallationList.reveal(vm);
				}
				fireSelectionChanged();
			}
		}
	}

	/**
	 * Fire current selection
	 */
	private void fireSelectionChanged() {
		final SelectionChangedEvent event = new SelectionChangedEvent(this,
				getSelection());
		final Object[] listeners = fSelectionListeners.getListeners();
		for (final Object element : listeners) {
			final ISelectionChangedListener listener = (ISelectionChangedListener) element;
			listener.selectionChanged(event);
		}
	}

	/**
	 * Sorts by VM type, and name within type.
	 */
	protected void sortByVersion() {
		fInstallationList.setSorter(new ViewerSorter() {

			@Override
			public int compare(Viewer viewer, Object e1, Object e2) {
				if ((e1 instanceof InstallationInfo)
						&& (e2 instanceof InstallationInfo)) {
					final InstallationInfo left = (InstallationInfo) e1;
					final InstallationInfo right = (InstallationInfo) e2;
					final String leftType = left.getVersion();
					final String rightType = right.getVersion();
					final int res = leftType.compareToIgnoreCase(rightType);
					if (res != 0) {
						return res;
					}
					return left.getName().compareToIgnoreCase(right.getName());
				}
				return super.compare(viewer, e1, e2);
			}

			@Override
			public boolean isSorterProperty(Object element, String property) {
				return true;
			}
		});
		fSortColumn = 3;
	}

	/**
	 * Sorts by VM name.
	 */
	protected void sortByName() {
		fInstallationList.setSorter(new ViewerSorter() {

			@Override
			public int compare(Viewer viewer, Object e1, Object e2) {
				if ((e1 instanceof InstallationInfo)
						&& (e2 instanceof InstallationInfo)) {
					final InstallationInfo left = (InstallationInfo) e1;
					final InstallationInfo right = (InstallationInfo) e2;
					return left.getName().compareToIgnoreCase(right.getName());
				}
				return super.compare(viewer, e1, e2);
			}

			@Override
			public boolean isSorterProperty(Object element, String property) {
				return true;
			}
		});
		fSortColumn = 1;
	}

	/**
	 * Sorts by VM location.
	 */
	protected void sortByLocation() {
		fInstallationList.setSorter(new ViewerSorter() {

			@Override
			public int compare(Viewer viewer, Object e1, Object e2) {
				if ((e1 instanceof InstallationInfo)
						&& (e2 instanceof InstallationInfo)) {
					final InstallationInfo left = (InstallationInfo) e1;
					final InstallationInfo right = (InstallationInfo) e2;
					return left.getOtpHome().compareToIgnoreCase(
							right.getOtpHome());
				}
				return super.compare(viewer, e1, e2);
			}

			@Override
			public boolean isSorterProperty(Object element, String property) {
				return true;
			}
		});
		fSortColumn = 2;
	}

	protected void enableButtons() {
		final int selectionCount = ((IStructuredSelection) fInstallationList
				.getSelection()).size();
		fEditButton.setEnabled(selectionCount == 1);
		fRemoveButton
				.setEnabled(selectionCount > 0
						&& selectionCount < fInstallationList.getTable()
								.getItemCount());
	}

	protected Button createPushButton(Composite parent, String label) {
		return SWTUtil.createPushButton(parent, label, null);
	}

	/**
	 * Correctly resizes the table so no phantom columns appear
	 */
	protected void configureTableResizing(final Composite parent,
			final Composite buttons, final Table table,
			final TableColumn column1, final TableColumn column2,
			final TableColumn column3) {
		parent.addControlListener(new ControlAdapter() {

			@Override
			public void controlResized(ControlEvent e) {
				resizeTable(parent, buttons, table, column1, column2, column3);
			}
		});
		table.addListener(SWT.Paint, new Listener() {

			public void handleEvent(Event event) {
				table.removeListener(SWT.Paint, this);
				resizeTable(parent, buttons, table, column1, column2, column3);
			}
		});
		column1.addControlListener(new ControlAdapter() {

			@Override
			public void controlResized(ControlEvent e) {
				if (column1.getWidth() > 0 && !fResizingTable) {
					fWeight1 = getColumnWeight(0);
				}
			}
		});
		column2.addControlListener(new ControlAdapter() {

			@Override
			public void controlResized(ControlEvent e) {
				if (column2.getWidth() > 0 && !fResizingTable) {
					fWeight2 = getColumnWeight(1);
				}
			}
		});
	}

	protected void resizeTable(Composite parent, Composite buttons,
			Table table, TableColumn column1, TableColumn column2,
			TableColumn column3) {
		fResizingTable = true;
		int parentWidth = -1;
		int parentHeight = -1;
		if (parent.isVisible()) {
			final Rectangle area = parent.getClientArea();
			parentWidth = area.width;
			parentHeight = area.height;
		} else {
			final Point parentSize = parent.computeSize(SWT.DEFAULT,
					SWT.DEFAULT);
			parentWidth = parentSize.x;
			parentHeight = parentSize.y;
		}
		final Point preferredSize = table.computeSize(SWT.DEFAULT, SWT.DEFAULT);
		int width = parentWidth - 2 * table.getBorderWidth();
		if (preferredSize.y > parentHeight) {
			// Subtract the scrollbar width from the total column width
			// if a vertical scrollbar will be required
			final Point vBarSize = table.getVerticalBar().getSize();
			width -= vBarSize.x;
		}
		width -= buttons.getSize().x;
		final Point oldSize = table.getSize();
		if (oldSize.x > width) {
			// table is getting smaller so make the columns
			// smaller first and then resize the table to
			// match the client area width
			column1.setWidth(Math.round(width * fWeight1));
			column2.setWidth(Math.round(width * fWeight2));
			column3.setWidth(width - (column1.getWidth() + column2.getWidth()));
			table.setSize(width, parentHeight);
		} else {
			// table is getting bigger so make the table
			// bigger first and then make the columns wider
			// to match the client area width
			table.setSize(width, parentHeight);
			column1.setWidth(Math.round(width * fWeight1));
			column2.setWidth(Math.round(width * fWeight2));
			column3.setWidth(width - (column1.getWidth() + column2.getWidth()));
		}
		fResizingTable = false;
	}

	/**
	 * Returns the Runtimes currently being displayed in this block
	 * 
	 * @return Runtimes currently being displayed in this block
	 */
	public List<InstallationInfo> getInstallations() {
		return new ArrayList<InstallationInfo>(installations);
	}

	/**
	 * Bring up a dialog that lets the user create a new VM definition.
	 */
	protected void addInstallation() {
		fInstallationList.refresh();

		final AddInstallationDialog dialog = new AddInstallationDialog(this,
				getShell(), null);
		dialog.setTitle(InstallationPreferenceMessages.add_title);
		if (dialog.open() != Window.OK) {
			return;
		}
		fInstallationList.refresh();
	}

	/**
	 * @see IAddRuntimeDialogRequestor#itemAdded(InstallationInfo)
	 */
	public void itemAdded(InstallationInfo installation) {
		installations.add(installation);
		fInstallationList.refresh();
	}

	/**
	 * @see IAddRuntimeDialogRequestor#isDuplicateName(String)
	 */
	public boolean isDuplicateName(String name) {
		return InstallationInfoManager.getDefault().isDuplicateName(name);
	}

	protected void editInstallation() {
		final IStructuredSelection selection = (IStructuredSelection) fInstallationList
				.getSelection();
		final InstallationInfo vm = (InstallationInfo) selection
				.getFirstElement();
		if (vm == null) {
			return;
		}
		final AddInstallationDialog dialog = new AddInstallationDialog(this,
				getShell(), vm);
		dialog.setTitle(InstallationPreferenceMessages.edit_title);
		if (dialog.open() != Window.OK) {
			return;
		}
		fInstallationList.refresh(vm);
	}

	protected void removeSelectedInstallations() {
		final IStructuredSelection selection = (IStructuredSelection) fInstallationList
				.getSelection();
		final InstallationInfo[] vms = new InstallationInfo[selection.size()];
		final Iterator<?> iter = selection.iterator();
		int i = 0;
		while (iter.hasNext()) {
			vms[i] = (InstallationInfo) iter.next();
			i++;
		}
		removeInstallations(vms);
	}

	/**
	 * Removes the given VMs from the table.
	 * 
	 * @param vms
	 */
	public void removeInstallations(InstallationInfo[] vms) {
		final IStructuredSelection prev = (IStructuredSelection) getSelection();
		for (final InstallationInfo rt : vms) {
			installations.remove(rt);
			for (RuntimeInfo bi : rt.getRuntimes()) {
				RuntimeInfoManager.getDefault().removeElement(bi.getName());
			}
		}
		fInstallationList.refresh();
		final IStructuredSelection curr = (IStructuredSelection) getSelection();
		if (!curr.equals(prev)) {
			final List<InstallationInfo> installs = getInstallations();
			if (curr.size() == 0 && installs.size() == 1) {
				// pick a default VM automatically
				setSelection(new StructuredSelection(installs.get(0)));
			} else {
				fireSelectionChanged();
			}
		}
	}

	/**
	 * Search for installed VMs in the file system
	 */
	protected void search() {

		// choose a root directory for the search
		final DirectoryDialog dialog = new DirectoryDialog(getShell());
		dialog.setMessage(InstallationPreferenceMessages.search_message);
		dialog.setText(InstallationPreferenceMessages.search_text);
		final String path = dialog.open();
		if (path == null) {
			return;
		}

		// ignore installed locations
		final Set<String> existingLocations = new HashSet<String>();
		{
			for (InfoElement rt : installations) {
				existingLocations.add(((InstallationInfo) rt).getOtpHome());
			}
		}

		// search
		final File rootDir = new File(path);
		final List<File> locations = new ArrayList<File>();

		final IRunnableWithProgress r = new IRunnableWithProgress() {

			public void run(IProgressMonitor monitor) {
				monitor.beginTask(InstallationPreferenceMessages.search_task,
						IProgressMonitor.UNKNOWN);
				search(rootDir, locations, existingLocations, monitor);
				monitor.done();
			}
		};

		try {
			final ProgressMonitorDialog progress = new ProgressMonitorDialog(
					getShell());
			progress.run(true, true, r);
		} catch (final InvocationTargetException e) {
			ErlideUIPlugin.log(e);
		} catch (final InterruptedException e) {
			// Canceled
			return;
		}

		if (locations.isEmpty()) {
			MessageDialog.openInformation(getShell(),
					InstallationPreferenceMessages.info_title,
					MessageFormat.format(
							InstallationPreferenceMessages.info_message,
							(Object[]) new String[] { path }));
		} else {
			Iterator<File> iter = locations.iterator();
			while (iter.hasNext()) {
				final File location = iter.next();
				final InstallationInfo vm = new InstallationInfo();
				final String name = location.getName();
				String nameCopy = name;
				int i = 1;
				while (isDuplicateName(nameCopy)) {
					nameCopy = name + '(' + i++ + ')';
				}
				vm.setName(nameCopy);
				vm.setOtpHome(location.getAbsolutePath());
				itemAdded(vm);
			}
		}

	}

	@Override
	public Shell getShell() {
		return getControl().getShell();
	}

	/**
	 * Searches the specified directory recursively for installed VMs, adding
	 * each detected VM to the <code>found</code> list. Any directories
	 * specified in the <code>ignore</code> are not traversed.
	 * 
	 * @param directory
	 * @param found
	 * @param types
	 * @param ignore
	 */
	protected void search(File directory, List<File> found, Set<String> ignore,
			IProgressMonitor monitor) {
		if (monitor.isCanceled()) {
			return;
		}

		final String[] names = directory.list();
		if (names == null) {
			return;
		}
		final List<File> subDirs = new ArrayList<File>();
		for (final String element : names) {
			if (monitor.isCanceled()) {
				return;
			}
			final File file = new File(directory, element);
			try {
				monitor.subTask(MessageFormat.format("searching...",
						(Object[]) new String[] {
								Integer.toString(found.size()),
								file.getCanonicalPath() }));
			} catch (final IOException e) {
			}
			if (file.isDirectory()) {
				if (!ignore.contains(file.getName())) {
					boolean validLocation = InstallationInfo
							.isValidOtpHome(file.getAbsolutePath());

					// FIXME

					if (!validLocation) {
						subDirs.add(file);
					}
				}
			}
		}
		while (!subDirs.isEmpty()) {
			final File subDir = subDirs.remove(0);
			search(subDir, found, ignore, monitor);
			if (monitor.isCanceled()) {
				return;
			}
		}

	}

	/**
	 * Sets the checked Runtime, possible <code>null</code>
	 * 
	 * @param vm
	 *            Runtime or <code>null</code>
	 */
	public void setCheckedInstallation(InstallationInfo vm) {
		if (vm == null) {
			setSelection(new StructuredSelection());
		} else {
			setSelection(new StructuredSelection(vm));
		}
	}

	/**
	 * Returns the checked Runtime or <code>null</code> if none.
	 * 
	 * @return the checked Runtime or <code>null</code> if none
	 */
	public InstallationInfo getCheckedInstallation() {
		final Object[] objects = fInstallationList.getCheckedElements();
		if (objects.length == 0) {
			return null;
		}
		return (InstallationInfo) objects[0];
	}

	/**
	 * Persist table settings into the give dialog store, prefixed with the
	 * given key.
	 * 
	 * @param settings
	 *            dialog store
	 * @param qualifier
	 *            key qualifier
	 */
	public void saveColumnSettings(IDialogSettings settings, String qualifier) {
		for (int i = 0; i < 2; i++) {
			// persist the first 2 column weights
			settings.put(qualifier + ".column" + i, getColumnWeight(i)); //$NON-NLS-1$
		}
		settings.put(qualifier + ".sortColumn", fSortColumn); //$NON-NLS-1$
	}

	protected float getColumnWeight(int col) {
		final Table table = fInstallationList.getTable();
		final int tableWidth = table.getSize().x;
		final int columnWidth = table.getColumn(col).getWidth();
		if (tableWidth > columnWidth) {
			return ((float) columnWidth) / tableWidth;
		}
		return 1 / 3F;
	}

	/**
	 * Restore table settings from the given dialog store using the given key.
	 * 
	 * @param settings
	 *            dialog settings store
	 * @param qualifier
	 *            key to restore settings from
	 */
	public void restoreColumnSettings(IDialogSettings settings, String qualifier) {
		fWeight1 = restoreColumnWeight(settings, qualifier, 0);
		fWeight2 = restoreColumnWeight(settings, qualifier, 1);
		fInstallationList.getTable().layout(true);
		try {
			fSortColumn = settings.getInt(qualifier + ".sortColumn"); //$NON-NLS-1$
		} catch (final NumberFormatException e) {
			fSortColumn = 1;
		}
		switch (fSortColumn) {
		case 1:
			sortByName();
			break;
		case 2:
			sortByLocation();
			break;
		case 3:
			sortByVersion();
			break;
		}
	}

	private float restoreColumnWeight(IDialogSettings settings,
			String qualifier, int col) {
		try {
			return settings.getFloat(qualifier + ".column" + col); //$NON-NLS-1$
		} catch (final NumberFormatException e) {
			return 1 / 3F;
		}

	}

	@Override
	protected Control createContents(Composite parent) {
		initializeDialogUnits(parent);

		final GridLayout layout = new GridLayout();
		layout.numColumns = 1;
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		parent.setLayout(layout);

		final Control ctrl = createMyControl(parent);
		final GridData data = new GridData(GridData.FILL_BOTH);
		data.horizontalSpan = 1;
		ctrl.setLayoutData(data);

		addSelectionChangedListener(new ISelectionChangedListener() {

			public void selectionChanged(SelectionChangedEvent event) {
				checkValid();
			}
		});

		checkValid();

		applyDialogFont(parent);
		return parent;
	}

	public void init(IWorkbench workbench) {
	}

	private Control createMyControl(Composite ancestor) {
		final Composite parent = new Composite(ancestor, SWT.NULL);
		GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		parent.setLayout(layout);
		final Font font = ancestor.getFont();
		parent.setFont(font);

		GridData data;

		tableLabel = new Label(parent, SWT.NONE);
		tableLabel.setText(InstallationPreferenceMessages.installations);
		data = new GridData();
		data.horizontalSpan = 2;
		tableLabel.setLayoutData(data);
		tableLabel.setFont(font);

		final Table table = new Table(parent, SWT.CHECK | SWT.BORDER
				| SWT.MULTI | SWT.FULL_SELECTION);
		table.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				if (e.detail == SWT.CHECK) {
					InstallationInfo ri = (InstallationInfo) e.item.getData();
					defaultInstallation = ri;
				}
			}
		});

		data = new GridData(GridData.FILL_BOTH);
		table.setLayoutData(data);
		table.setFont(font);

		table.setHeaderVisible(true);
		table.setLinesVisible(true);

		final TableLayout tableLayout = new TableLayout();
		table.setLayout(tableLayout);

		final TableColumn column1 = new TableColumn(table, SWT.NULL);
		column1.setWidth(80);
		column1.setText(InstallationPreferenceMessages.name);
		column1.setResizable(true);
		column1.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				sortByName();
			}
		});

		final TableColumn column2 = new TableColumn(table, SWT.NULL);
		column2.setWidth(150);
		column2.setText(InstallationPreferenceMessages.location);
		column2.setResizable(true);
		column2.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				sortByLocation();
			}
		});

		final TableColumn column3 = new TableColumn(table, SWT.NULL);
		column3.setWidth(80);
		column3.setText(InstallationPreferenceMessages.version);
		column3.setResizable(false);
		column3.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				sortByVersion();
			}
		});

		fInstallationList = new CheckboxTableViewer(table);
		fInstallationList.setLabelProvider(new InstallationLabelProvider());
		fInstallationList.setContentProvider(new InstallationContentProvider());
		fInstallationList.setInput(installations);
		if (defaultInstallation != null) {
			fInstallationList
					.setCheckedElements(new Object[] { defaultInstallation });
		}
		fInstallationList.addCheckStateListener(new ICheckStateListener() {

			public void checkStateChanged(CheckStateChangedEvent event) {
				InstallationInfo e = (InstallationInfo) event.getElement();
				if (event.getChecked()) {
					setCheckedInstallation(e);
				}
			}
		});
		// by default, sort by name
		sortByName();

		fInstallationList
				.addSelectionChangedListener(new ISelectionChangedListener() {

					public void selectionChanged(SelectionChangedEvent evt) {
						enableButtons();
					}
				});

		fInstallationList.addCheckStateListener(new ICheckStateListener() {

			public void checkStateChanged(CheckStateChangedEvent event) {
				if (event.getChecked()) {
					setCheckedInstallation((InstallationInfo) event
							.getElement());
				} else {
					setCheckedInstallation(null);
				}
			}
		});

		fInstallationList.addDoubleClickListener(new IDoubleClickListener() {

			public void doubleClick(DoubleClickEvent e) {
				if (!fInstallationList.getSelection().isEmpty()) {
					editInstallation();
				}
			}
		});
		table.addKeyListener(new KeyAdapter() {

			@Override
			public void keyPressed(KeyEvent event) {
				if (event.character == SWT.DEL && event.stateMask == 0) {
					removeSelectedInstallations();
				}
			}
		});

		buttons = new Composite(parent, SWT.NULL);
		final GridData gd_buttons = new GridData(SWT.LEFT, SWT.TOP, false,
				false);
		gd_buttons.heightHint = 133;
		buttons.setLayoutData(gd_buttons);
		layout = new GridLayout();
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		buttons.setLayout(layout);
		buttons.setFont(font);

		fAddButton = createPushButton(buttons,
				InstallationPreferenceMessages.add);
		fAddButton.addListener(SWT.Selection, new Listener() {

			public void handleEvent(Event evt) {
				addInstallation();
			}
		});

		fEditButton = createPushButton(buttons,
				InstallationPreferenceMessages.edit);
		fEditButton.addListener(SWT.Selection, new Listener() {

			public void handleEvent(Event evt) {
				editInstallation();
			}
		});

		fRemoveButton = createPushButton(buttons,
				InstallationPreferenceMessages.remove);
		fRemoveButton.addListener(SWT.Selection, new Listener() {

			public void handleEvent(Event evt) {
				removeSelectedInstallations();
			}
		});

		// copied from ListDialogField.CreateSeparator()
		final Label separator = new Label(buttons, SWT.NONE);
		separator.setVisible(false);
		final GridData gd = new GridData();
		gd.horizontalAlignment = GridData.FILL;
		gd.verticalAlignment = GridData.BEGINNING;
		gd.heightHint = 4;
		separator.setLayoutData(gd);

		fSearchButton = createPushButton(buttons,
				InstallationPreferenceMessages.search);
		fSearchButton.addListener(SWT.Selection, new Listener() {

			public void handleEvent(Event evt) {
				search();
			}
		});

		configureTableResizing(parent, buttons, table, column1, column2,
				column3);
		parent.setTabList(new Control[] { tableLabel, table, buttons });

		enableButtons();

		return parent;
	}

	@Override
	public boolean performOk() {
		saveData();
		return super.performOk();
	}

	private void saveData() {
		InstallationInfoManager.getDefault().setElements(installations);
		for (InstallationInfo rt : installations) {
			RuntimeInfoManager.getDefault().createDefaultRuntimes(rt);
		}
		if (defaultInstallation != null) {
			InstallationInfoManager.getDefault().setSelectedKey(
					defaultInstallation.getName());
		}

		// save column widths
		final IDialogSettings settings = ErlideUIPlugin.getDefault()
				.getDialogSettings();
		saveColumnSettings(settings, INSTALLATIONS_PREFERENCE_PAGE);
	}

	@Override
	public void performApply() {
		saveData();
		super.performApply();
	}

	@Override
	protected void performDefaults() {
		refreshData();
		fInstallationList.refresh();
		super.performDefaults();
	}

	void checkValid() {
		final InstallationInfo def = getCheckedInstallation();
		if (def == null && getInstallations().size() > 0) {
			setValid(false);
			setErrorMessage(InstallationPreferenceMessages.Page_pleaseSelectADefaultInstallation);
		} else {
			setValid(true);
			setErrorMessage(null);
		}
	}

}
