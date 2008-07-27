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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.ListenerList;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ComboViewer;
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
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.erlide.basicui.ErlideBasicUIPlugin;
import org.erlide.basicui.prefs.IAddDialogRequestor;
import org.erlide.basicui.prefs.PreferenceMessages;
import org.erlide.basicui.util.SWTUtil;
import org.erlide.runtime.backend.BackendInfo;
import org.erlide.runtime.backend.BackendInfoManager;

/**
 * A preference page that displays installed runtimes in a table. Runtimes can
 * be added, removed, edited, and searched for.
 * <p>
 * It implements ISelectionProvider - it sends selection change events when the
 * checked runtime in the table changes, or when the "use default" button check
 * state changes.
 * </p>
 */
public class BackendsPreferencePage extends PreferencePage implements
		IAddDialogRequestor<BackendInfo>, ISelectionProvider,
		IWorkbenchPreferencePage {

	private Combo combo;
	private static final String BACKENDS_PREFERENCE_PAGE = "BACKENDS_PREFERENCE_PAGE";

	Collection<BackendInfo> backends;
	BackendInfo defaultBackend;
	BackendInfo erlideBackend;

	/**
	 * The main list control
	 */
	protected CheckboxTableViewer fBackendList;

	// Action buttons
	private Button fAddButton;
	private Button fRemoveButton;
	private Button fEditButton;

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
	private ComboViewer erlideBackendViewer;

	/**
	 * Content provider to show a list of ERTSs
	 */
	class BackendContentProvider implements IStructuredContentProvider {

		public Object[] getElements(Object input) {
			return backends.toArray(new BackendInfo[backends.size()]);
		}

		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		}

		public void dispose() {
		}

	}

	/**
	 * Label provider for installed runtimes table.
	 */
	static class BackendLabelProvider extends LabelProvider implements
			ITableLabelProvider {

		/**
		 * @see ITableLabelProvider#getColumnText(Object, int)
		 */
		public String getColumnText(Object element, int columnIndex) {
			if (element instanceof BackendInfo) {
				final BackendInfo vm = (BackendInfo) element;
				switch (columnIndex) {
				case 0:
					return vm.getName();
				case 1:
					return vm.getRuntime();
				case 2:
					return vm.getNodeName();
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

		@Override
		public String getText(Object element) {
			return getColumnText(element, 0);
		}
	}

	public BackendsPreferencePage() {
		super();
		setTitle("Installed Erlang backends ");
		setDescription("Add, remove or edit backend definitions.\r\n"
				+ "The checked backend will be used by default in new projects "
				+ "to build and run the project's code.");

		performDefaults();
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
		return new StructuredSelection(fBackendList.getCheckedElements());
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
					fBackendList.setCheckedElements(new Object[0]);
				} else {
					fBackendList.setCheckedElements(new Object[] { vm });
					fBackendList.reveal(vm);
				}
				fireSelectionChanged();
			}
		}
	}

	/**
	 * Fire current selection
	 */
	void fireSelectionChanged() {
		final SelectionChangedEvent event = new SelectionChangedEvent(this,
				getSelection());
		final Object[] listeners = fSelectionListeners.getListeners();
		for (final Object element : listeners) {
			final ISelectionChangedListener listener = (ISelectionChangedListener) element;
			listener.selectionChanged(event);
		}
	}

	/**
	 * Sorts by VM name.
	 */
	protected void sortByName() {
		fBackendList.setSorter(new ViewerSorter() {

			@Override
			public int compare(Viewer viewer, Object e1, Object e2) {
				if ((e1 instanceof BackendInfo) && (e2 instanceof BackendInfo)) {
					final BackendInfo left = (BackendInfo) e1;
					final BackendInfo right = (BackendInfo) e2;
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
	protected void sortByRuntime() {
		fBackendList.setSorter(new ViewerSorter() {

			@Override
			public int compare(Viewer viewer, Object e1, Object e2) {
				if ((e1 instanceof BackendInfo) && (e2 instanceof BackendInfo)) {
					final BackendInfo left = (BackendInfo) e1;
					final BackendInfo right = (BackendInfo) e2;
					return left.getRuntime().compareToIgnoreCase(
							right.getRuntime());
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
		final int selectionCount = ((IStructuredSelection) fBackendList
				.getSelection()).size();
		fEditButton.setEnabled(selectionCount == 1);
		fRemoveButton.setEnabled(selectionCount > 0
				&& selectionCount < fBackendList.getTable().getItemCount());
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
	 * Returns the ERTSs currently being displayed in this block
	 * 
	 * @return ERTSs currently being displayed in this block
	 */
	public List<BackendInfo> getBackends() {
		return new ArrayList<BackendInfo>(backends);
	}

	/**
	 * Bring up a dialog that lets the user create a new backend definition.
	 */
	protected void addBackend() {
		fBackendList.refresh();

		final AddBackendDialog dialog = new AddBackendDialog(this, getShell(),
				null);
		dialog.setTitle(PreferenceMessages.InstalledERTSsBlock_7);
		if (dialog.open() != Window.OK) {
			return;
		}
		fBackendList.refresh();
		erlideBackendViewer.refresh();
	}

	public void itemAdded(BackendInfo vm) {
		backends.add(vm);
		fBackendList.refresh();
		erlideBackendViewer.refresh();
	}

	/**
	 * @see IAddRuntimeDialogRequestor#isDuplicateName(String)
	 */
	public boolean isDuplicateName(String name) {
		return BackendInfoManager.getDefault().isDuplicateName(name);
	}

	protected void editBackend() {
		final IStructuredSelection selection = (IStructuredSelection) fBackendList
				.getSelection();
		final BackendInfo vm = (BackendInfo) selection.getFirstElement();
		if (vm == null) {
			return;
		}
		final AddBackendDialog dialog = new AddBackendDialog(this, getShell(),
				vm);
		dialog.setTitle(PreferenceMessages.InstalledERTSsBlock_8);
		if (dialog.open() != Window.OK) {
			return;
		}
		fBackendList.refresh(vm);
	}

	protected void removeSelectedBackends() {
		final IStructuredSelection selection = (IStructuredSelection) fBackendList
				.getSelection();
		final BackendInfo[] vms = new BackendInfo[selection.size()];
		final Iterator<?> iter = selection.iterator();
		int i = 0;
		while (iter.hasNext()) {
			vms[i] = (BackendInfo) iter.next();
			i++;
		}
		removeBackends(vms);
	}

	/**
	 * Removes the given VMs from the table.
	 * 
	 * @param vms
	 */
	public void removeBackends(BackendInfo[] vms) {
		final IStructuredSelection prev = (IStructuredSelection) getSelection();
		for (final BackendInfo element : vms) {
			backends.remove(element);
		}
		fBackendList.refresh();
		erlideBackendViewer.refresh();
		final IStructuredSelection curr = (IStructuredSelection) getSelection();
		if (!curr.equals(prev)) {
			final List<BackendInfo> installs = getBackends();
			if (curr.size() == 0 && installs.size() == 1) {
				// pick a default VM automatically
				setSelection(new StructuredSelection(installs.get(0)));
			} else {
				fireSelectionChanged();
			}
		}
	}

	@Override
	public Shell getShell() {
		return getControl().getShell();
	}

	/**
	 * Sets the checked ERTS, possible <code>null</code>
	 * 
	 * @param vm
	 *            ERTS or <code>null</code>
	 */
	public void setCheckedBackend(BackendInfo vm) {
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
	public BackendInfo getCheckedBackend() {
		final Object[] objects = fBackendList.getCheckedElements();
		if (objects.length == 0) {
			return null;
		}
		return (BackendInfo) objects[0];
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
		final Table table = fBackendList.getTable();
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
		fBackendList.getTable().layout(true);
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
			sortByRuntime();
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

		final Composite composite = new Composite(parent, SWT.NONE);
		composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, false));
		final GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 2;
		composite.setLayout(gridLayout);

		final Label erlideWillUseLabel = new Label(composite, SWT.NONE);
		erlideWillUseLabel
				.setToolTipText("The erlide backend is used for IDE purposes, not for running project code.");
		erlideWillUseLabel
				.setText("Erlide backend (requires restart to be effective!)");

		erlideBackendViewer = new ComboViewer(composite, SWT.READ_ONLY);
		erlideBackendViewer.setLabelProvider(new BackendLabelProvider());
		erlideBackendViewer.setContentProvider(new BackendContentProvider());
		erlideBackendViewer.setInput(backends);
		combo = erlideBackendViewer.getCombo();
		final GridData gd_combo = new GridData(SWT.FILL, SWT.FILL, true, false);
		gd_combo.widthHint = 118;
		combo.setLayoutData(gd_combo);
		if (erlideBackend != null) {
			erlideBackendViewer.setSelection(new StructuredSelection(
					erlideBackend), true);
		}
		erlideBackendViewer
				.addPostSelectionChangedListener(new ISelectionChangedListener() {
					public void selectionChanged(
							final SelectionChangedEvent event) {
						fireSelectionChanged();
					}
				});
		erlideBackendViewer
				.addSelectionChangedListener(new ISelectionChangedListener() {
					public void selectionChanged(SelectionChangedEvent event) {
						ISelection sel = event.getSelection();
						if (sel instanceof IStructuredSelection) {
							IStructuredSelection ssel = (IStructuredSelection) sel;
							erlideBackend = (BackendInfo) ssel
									.getFirstElement();
						}

					}
				});

		final Label tableLabel = new Label(parent, SWT.NONE);
		tableLabel.setText("Installed backends:");
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
					BackendInfo ri = (BackendInfo) e.item.getData();
					defaultBackend = ri;
				}
			}
		});

		data = new GridData(SWT.FILL, SWT.FILL, true, true);
		data.widthHint = 403;
		table.setLayoutData(data);
		table.setFont(font);

		table.setHeaderVisible(true);
		table.setLinesVisible(true);

		final TableLayout tableLayout = new TableLayout();
		table.setLayout(tableLayout);

		final TableColumn column1 = new TableColumn(table, SWT.NULL);
		column1.setWidth(80);
		column1.setText(PreferenceMessages.InstalledERTSsBlock_0);
		column1.setResizable(true);
		column1.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				sortByName();
			}
		});

		final TableColumn column2 = new TableColumn(table, SWT.NULL);
		column2.setWidth(150);
		column2.setText("Runtime");
		column2.setResizable(true);
		column2.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				sortByRuntime();
			}
		});

		final TableColumn column3 = new TableColumn(table, SWT.NULL);
		column3.setWidth(80);
		column3.setText("Node name");
		column3.setResizable(false);

		fBackendList = new CheckboxTableViewer(table);
		fBackendList.setLabelProvider(new BackendLabelProvider());
		fBackendList.setContentProvider(new BackendContentProvider());
		fBackendList.setInput(backends);
		if (defaultBackend != null) {
			fBackendList.setCheckedElements(new Object[] { defaultBackend });
		}
		// by default, sort by name
		sortByName();

		fBackendList
				.addSelectionChangedListener(new ISelectionChangedListener() {

					public void selectionChanged(SelectionChangedEvent evt) {
						enableButtons();
					}
				});

		fBackendList.addCheckStateListener(new ICheckStateListener() {

			public void checkStateChanged(CheckStateChangedEvent event) {
				if (event.getChecked()) {
					setCheckedBackend((BackendInfo) event.getElement());
				} else {
					setCheckedBackend(null);
				}
			}
		});

		fBackendList.addDoubleClickListener(new IDoubleClickListener() {

			public void doubleClick(DoubleClickEvent e) {
				if (!fBackendList.getSelection().isEmpty()) {
					editBackend();
				}
			}
		});
		table.addKeyListener(new KeyAdapter() {

			@Override
			public void keyPressed(KeyEvent event) {
				if (event.character == SWT.DEL && event.stateMask == 0) {
					removeSelectedBackends();
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
				PreferenceMessages.InstalledERTSsBlock_3);
		fAddButton.addListener(SWT.Selection, new Listener() {

			public void handleEvent(Event evt) {
				addBackend();
			}
		});

		fEditButton = createPushButton(buttons,
				PreferenceMessages.InstalledERTSsBlock_4);
		fEditButton.addListener(SWT.Selection, new Listener() {

			public void handleEvent(Event evt) {
				editBackend();
			}
		});

		fRemoveButton = createPushButton(buttons,
				PreferenceMessages.InstalledERTSsBlock_5);
		fRemoveButton.addListener(SWT.Selection, new Listener() {

			public void handleEvent(Event evt) {
				removeSelectedBackends();
			}
		});

		configureTableResizing(parent, buttons, table, column1, column2,
				column3);

		enableButtons();

		return parent;
	}

	@Override
	public boolean performOk() {
		BackendInfoManager.getDefault().setErlideBackend(erlideBackend);
		BackendInfoManager.getDefault()
				.setSelectedKey(defaultBackend.getName());
		BackendInfoManager.getDefault().setElements(backends);

		// save column widths
		final IDialogSettings settings = ErlideBasicUIPlugin.getDefault()
				.getDialogSettings();
		saveColumnSettings(settings, BACKENDS_PREFERENCE_PAGE);

		return super.performOk();
	}

	@Override
	public void performDefaults() {
		backends = BackendInfoManager.getDefault().getElements();
		defaultBackend = BackendInfoManager.getDefault().getDefaultBackend();
		erlideBackend = BackendInfoManager.getDefault().getErlideBackend();
	}

	void checkValid() {
		final BackendInfo def = getCheckedBackend();
		StructuredSelection sel = (StructuredSelection) erlideBackendViewer
				.getSelection();

		if (def == null && getBackends().size() > 0) {
			setValid(false);
			setErrorMessage("Please select a default backend.");
		} else if (sel.isEmpty() && getBackends().size() > 0) {
			setValid(false);
			setErrorMessage("Please select a backend to be used by erlide.");
		} else {
			setValid(true);
			setErrorMessage(null);
		}
	}
}
