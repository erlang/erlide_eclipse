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
package org.erlide.basicui.prefs;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.text.MessageFormat;
import java.util.ArrayList;
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
import org.eclipse.swt.custom.BusyIndicator;
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
import org.erlide.basiccore.ErtsInstall;
import org.erlide.basicui.ErlideBasicUIPlugin;
import org.erlide.basicui.util.SWTUtil;

/**
 * A composite that displays installed ERTS's in a table. ERTSs can be added,
 * removed, edited, and searched for.
 * <p>
 * This block implements ISelectionProvider - it sends selection change events
 * when the checked ERTS in the table changes, or when the "use default" button
 * check state changes.
 * </p>
 */
public class ErtsInstallPreferencePage extends PreferencePage implements
		IAddVMDialogRequestor, ISelectionProvider, IWorkbenchPreferencePage

{

	/**
	 * VMs being displayed
	 */
	protected List<ErtsInstall> fVMs = new ArrayList<ErtsInstall>();

	/**
	 * The main list control
	 */
	protected CheckboxTableViewer fVMList;

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
	 * Selection listeners (checked ERTS changes)
	 */
	private final ListenerList fSelectionListeners = new ListenerList();

	/**
	 * Previous selection
	 */
	private ISelection fPrevSelection = new StructuredSelection();

	/**
	 * Content provider to show a list of ERTSs
	 */
	class ERTSsContentProvider implements IStructuredContentProvider {

		public Object[] getElements(Object input) {
			return fVMs.toArray();
		}

		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		}

		public void dispose() {
		}

	}

	/**
	 * Label provider for installed ERTSs table.
	 */
	class VMLabelProvider extends LabelProvider implements ITableLabelProvider {

		/**
		 * @see ITableLabelProvider#getColumnText(Object, int)
		 */
		public String getColumnText(Object element, int columnIndex) {
			if (element instanceof ErtsInstall) {
				final ErtsInstall vm = (ErtsInstall) element;
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

	public ErtsInstallPreferencePage() {
		super();
		setTitle(ErtsMessages.ERTSsPreferencePage_1);
		setDescription(ErtsMessages.ERTSsPreferencePage_2);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
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
		return new StructuredSelection(fVMList.getCheckedElements());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
	 */
	public void removeSelectionChangedListener(
			ISelectionChangedListener listener) {
		fSelectionListeners.remove(listener);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
	 */
	public void setSelection(ISelection selection) {
		if (selection instanceof IStructuredSelection) {
			if (!selection.equals(fPrevSelection)) {
				fPrevSelection = selection;
				final Object jre = ((IStructuredSelection) selection)
						.getFirstElement();
				if (jre == null) {
					fVMList.setCheckedElements(new Object[0]);
				} else {
					fVMList.setCheckedElements(new Object[] { jre });
					fVMList.reveal(jre);
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
		fVMList.setSorter(new ViewerSorter() {

			@Override
			public int compare(Viewer viewer, Object e1, Object e2) {
				if ((e1 instanceof ErtsInstall) && (e2 instanceof ErtsInstall)) {
					final ErtsInstall left = (ErtsInstall) e1;
					final ErtsInstall right = (ErtsInstall) e2;
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
		fVMList.setSorter(new ViewerSorter() {

			@Override
			public int compare(Viewer viewer, Object e1, Object e2) {
				if ((e1 instanceof ErtsInstall) && (e2 instanceof ErtsInstall)) {
					final ErtsInstall left = (ErtsInstall) e1;
					final ErtsInstall right = (ErtsInstall) e2;
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
		fVMList.setSorter(new ViewerSorter() {

			@Override
			public int compare(Viewer viewer, Object e1, Object e2) {
				if ((e1 instanceof ErtsInstall) && (e2 instanceof ErtsInstall)) {
					final ErtsInstall left = (ErtsInstall) e1;
					final ErtsInstall right = (ErtsInstall) e2;
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
		final int selectionCount = ((IStructuredSelection) fVMList
				.getSelection()).size();
		fEditButton.setEnabled(selectionCount == 1);
		fRemoveButton.setEnabled(selectionCount > 0
				&& selectionCount < fVMList.getTable().getItemCount());
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
	 * Sets the ERTSs to be displayed in this block
	 * 
	 * @param vms
	 *            ERTSs to be displayed
	 */
	protected void setERTSs(ErtsInstall[] vms) {
		fVMs.clear();
		for (final ErtsInstall element : vms) {
			fVMs.add(element);
		}
		fVMList.setInput(fVMs);
		fVMList.refresh();
	}

	/**
	 * Returns the ERTSs currently being displayed in this block
	 * 
	 * @return ERTSs currently being displayed in this block
	 */
	public ErtsInstall[] getERTSs() {
		return fVMs.toArray(new ErtsInstall[fVMs.size()]);
	}

	/**
	 * Bring up a dialog that lets the user create a new VM definition.
	 */
	protected void addVM() {
		fVMList.refresh();

		final AddVMDialog dialog = new AddVMDialog(this, getShell(), null);
		dialog.setTitle(ErtsMessages.InstalledERTSsBlock_7);
		if (dialog.open() != Window.OK) {
			return;
		}
		fVMList.refresh();
	}

	/**
	 * @see IAddVMDialogRequestor#vmAdded(ErtsInstall)
	 */
	public void vmAdded(ErtsInstall vm) {
		fVMs.add(vm);
		fVMList.refresh();
	}

	/**
	 * @see IAddVMDialogRequestor#isDuplicateName(String)
	 */
	public boolean isDuplicateName(String name) {
		for (int i = 0; i < fVMs.size(); i++) {
			final ErtsInstall vm = fVMs.get(i);
			if (vm.getName().equals(name)) {
				return true;
			}
		}
		return false;
	}

	protected void editVM() {
		final IStructuredSelection selection = (IStructuredSelection) fVMList
				.getSelection();
		final ErtsInstall vm = (ErtsInstall) selection.getFirstElement();
		if (vm == null) {
			return;
		}
		final AddVMDialog dialog = new AddVMDialog(this, getShell(), vm);
		dialog.setTitle(ErtsMessages.InstalledERTSsBlock_8);
		if (dialog.open() != Window.OK) {
			return;
		}
		fVMList.refresh(vm);
	}

	protected void removeVMs() {
		final IStructuredSelection selection = (IStructuredSelection) fVMList
				.getSelection();
		final ErtsInstall[] vms = new ErtsInstall[selection.size()];
		final Iterator iter = selection.iterator();
		int i = 0;
		while (iter.hasNext()) {
			vms[i] = (ErtsInstall) iter.next();
			i++;
		}
		removeERTSs(vms);
	}

	/**
	 * Removes the given VMs from the table.
	 * 
	 * @param vms
	 */
	public void removeERTSs(ErtsInstall[] vms) {
		final IStructuredSelection prev = (IStructuredSelection) getSelection();
		for (final ErtsInstall element : vms) {
			fVMs.remove(element);
		}
		fVMList.refresh();
		final IStructuredSelection curr = (IStructuredSelection) getSelection();
		if (!curr.equals(prev)) {
			final ErtsInstall[] installs = getERTSs();
			if (curr.size() == 0 && installs.length == 1) {
				// pick a default VM automatically
				setSelection(new StructuredSelection(installs[0]));
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
		dialog.setMessage(ErtsMessages.InstalledERTSsBlock_9);
		dialog.setText(ErtsMessages.InstalledERTSsBlock_10);
		final String path = dialog.open();
		if (path == null) {
			return;
		}

		// ignore installed locations
		final Set<String> exstingLocations = new HashSet<String>();
		{
			Iterator<ErtsInstall> iter = fVMs.iterator();
			while (iter.hasNext()) {
				exstingLocations.add((iter.next()).getOtpHome());
			}
		}

		// search
		final File rootDir = new File(path);
		final List<File> locations = new ArrayList<File>();

		final IRunnableWithProgress r = new IRunnableWithProgress() {

			public void run(IProgressMonitor monitor) {
				monitor.beginTask(ErtsMessages.InstalledERTSsBlock_11,
						IProgressMonitor.UNKNOWN);
				search(rootDir, locations, exstingLocations, monitor);
				monitor.done();
			}
		};

		try {
			final ProgressMonitorDialog progress = new ProgressMonitorDialog(
					getShell());
			progress.run(true, true, r);
		} catch (final InvocationTargetException e) {
			ErlideBasicUIPlugin.log(e);
		} catch (final InterruptedException e) {
			// cancelled
			return;
		}

		if (locations.isEmpty()) {
			MessageDialog.openInformation(getShell(),
					ErtsMessages.InstalledERTSsBlock_12, MessageFormat.format(
							ErtsMessages.InstalledERTSsBlock_13,
							(Object[]) new String[] { path }));
		} else {
			Iterator<File> iter = locations.iterator();
			while (iter.hasNext()) {
				final File location = iter.next();
				final ErtsInstall vm = new ErtsInstall();
				final String name = location.getName();
				String nameCopy = new String(name);
				int i = 1;
				while (isDuplicateName(nameCopy)) {
					nameCopy = name + '(' + i++ + ')';
				}
				vm.setName(nameCopy);
				vm.setOtpHome(location.getAbsolutePath());
				vmAdded(vm);
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
	protected void search(File directory, List found, Set ignore,
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
				if (!ignore.contains(file)) {
					boolean validLocation = false;

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
	 * Sets the checked ERTS, possible <code>null</code>
	 * 
	 * @param vm
	 *            ERTS or <code>null</code>
	 */
	public void setCheckedERTS(ErtsInstall vm) {
		if (vm == null) {
			setSelection(new StructuredSelection());
		} else {
			setSelection(new StructuredSelection(vm));
		}
	}

	/**
	 * Returns the checked ERTS or <code>null</code> if none.
	 * 
	 * @return the checked ERTS or <code>null</code> if none
	 */
	public ErtsInstall getCheckedERTS() {
		final Object[] objects = fVMList.getCheckedElements();
		if (objects.length == 0) {
			return null;
		}
		return (ErtsInstall) objects[0];
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
		final Table table = fVMList.getTable();
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
		fVMList.getTable().layout(true);
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

	/**
	 * Populates the ERTS table with existing ERTSs defined in the workspace.
	 */
	protected void fillWithWorkspaceERTSs() {
		setERTSs(new ErtsInstall[] {});
	}

	@Override
	protected Control createContents(Composite parent) {
		initializeDialogUnits(parent);

		noDefaultAndApplyButton();

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
				final ErtsInstall install = getCheckedERTS();
				if (install == null) {
					setValid(false);
					setErrorMessage(ErtsMessages.ERTSsPreferencePage_13);
				} else {
					setValid(true);
					setErrorMessage(null);
				}
			}
		});

		applyDialogFont(parent);
		return parent;
	}

	public void init(IWorkbench workbench) {

	}

	public Control createMyControl(Composite ancestor) {
		final Composite parent = new Composite(ancestor, SWT.NULL);
		GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		parent.setLayout(layout);
		final Font font = ancestor.getFont();
		parent.setFont(font);

		GridData data;

		final Label tableLabel = new Label(parent, SWT.NONE);
		tableLabel.setText(ErtsMessages.InstalledERTSsBlock_15);
		data = new GridData();
		data.horizontalSpan = 2;
		tableLabel.setLayoutData(data);
		tableLabel.setFont(font);

		final Table table = new Table(parent, SWT.CHECK | SWT.BORDER
				| SWT.MULTI | SWT.FULL_SELECTION);

		data = new GridData(GridData.FILL_BOTH);
		table.setLayoutData(data);
		table.setFont(font);

		table.setHeaderVisible(true);
		table.setLinesVisible(true);

		final TableLayout tableLayout = new TableLayout();
		table.setLayout(tableLayout);

		final TableColumn column1 = new TableColumn(table, SWT.NULL);
		column1.setText(ErtsMessages.InstalledERTSsBlock_0);
		column1.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				sortByName();
			}
		});

		final TableColumn column2 = new TableColumn(table, SWT.NULL);
		column2.setText(ErtsMessages.InstalledERTSsBlock_1);
		column2.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				sortByLocation();
			}
		});

		final TableColumn column3 = new TableColumn(table, SWT.NULL);
		column3.setText(ErtsMessages.InstalledERTSsBlock_2);
		column3.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				sortByVersion();
			}
		});

		fVMList = new CheckboxTableViewer(table);
		fVMList.setLabelProvider(new VMLabelProvider());
		fVMList.setContentProvider(new ERTSsContentProvider());
		fVMList.setInput(fVMs);
		// by default, sort by name
		sortByName();

		fVMList.addSelectionChangedListener(new ISelectionChangedListener() {

			public void selectionChanged(SelectionChangedEvent evt) {
				enableButtons();
			}
		});

		fVMList.addCheckStateListener(new ICheckStateListener() {

			public void checkStateChanged(CheckStateChangedEvent event) {
				if (event.getChecked()) {
					setCheckedERTS((ErtsInstall) event.getElement());
				} else {
					setCheckedERTS(null);
				}
			}
		});

		fVMList.addDoubleClickListener(new IDoubleClickListener() {

			public void doubleClick(DoubleClickEvent e) {
				if (!fVMList.getSelection().isEmpty()) {
					editVM();
				}
			}
		});
		table.addKeyListener(new KeyAdapter() {

			@Override
			public void keyPressed(KeyEvent event) {
				if (event.character == SWT.DEL && event.stateMask == 0) {
					removeVMs();
				}
			}
		});

		final Composite buttons = new Composite(parent, SWT.NULL);
		buttons.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_BEGINNING));
		layout = new GridLayout();
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		buttons.setLayout(layout);
		buttons.setFont(font);

		fAddButton = createPushButton(buttons,
				ErtsMessages.InstalledERTSsBlock_3);
		fAddButton.addListener(SWT.Selection, new Listener() {

			public void handleEvent(Event evt) {
				addVM();
			}
		});

		fEditButton = createPushButton(buttons,
				ErtsMessages.InstalledERTSsBlock_4);
		fEditButton.addListener(SWT.Selection, new Listener() {

			public void handleEvent(Event evt) {
				editVM();
			}
		});

		fRemoveButton = createPushButton(buttons,
				ErtsMessages.InstalledERTSsBlock_5);
		fRemoveButton.addListener(SWT.Selection, new Listener() {

			public void handleEvent(Event evt) {
				removeVMs();
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
				ErtsMessages.InstalledERTSsBlock_6);
		fSearchButton.addListener(SWT.Selection, new Listener() {

			public void handleEvent(Event evt) {
				search();
			}
		});

		configureTableResizing(parent, buttons, table, column1, column2,
				column3);

		fillWithWorkspaceERTSs();
		enableButtons();

		return parent;
	}

	@Override
	public boolean performOk() {
		final boolean[] canceled = new boolean[] { false };
		BusyIndicator.showWhile(null, new Runnable() {

			public void run() {
				// ErtsInstall defaultVM = getCheckedERTS();
				// ErtsInstall[] vms = getERTSs();
				// JREsUpdater updater = new JREsUpdater();
				// if (!updater.updateJRESettings(vms, defaultVM)) {
				// canceled[0] = true;
				// }
			}
		});

		if (canceled[0]) {
			return false;
		}

		// save column widths
		final IDialogSettings settings = ErlideBasicUIPlugin.getDefault()
				.getDialogSettings();
		// TODO fix arg
		saveColumnSettings(settings, "ERTS_PREFERENCE_PAGE");

		return super.performOk();
	}

}
