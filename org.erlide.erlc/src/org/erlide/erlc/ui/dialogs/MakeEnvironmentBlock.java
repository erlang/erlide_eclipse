/*******************************************************************************
 * Copyright (c) 2004, 2005 QNX Software Systems and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     QNX Software Systems - initial API and implementation
 *******************************************************************************/
package org.erlide.erlc.ui.dialogs;

import java.text.MessageFormat;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Preferences;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ColumnLayoutData;
import org.eclipse.jface.viewers.ColumnPixelData;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableLayout;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.ui.dialogs.ListSelectionDialog;
import org.erlide.core.ErlangPlugin;
import org.erlide.erlc.ErlideErlcPlugin;
import org.erlide.erlc.core.IMakeCommonBuildInfo;
import org.erlide.erlc.internal.DialogSettingsHelper;
import org.erlide.erlc.internal.ui.MultipleInputDialog;
import org.erlide.erlc.ui.controls.ControlFactory;
import org.erlide.erlc.utils.spawner.EnvironmentReader;

public class MakeEnvironmentBlock extends AbstractErlOptionPage {

	Preferences fPrefs;

	String fBuilderID;

	IMakeCommonBuildInfo fBuildInfo;

	protected TableViewer environmentTable;

	protected String[] envTableColumnHeaders = {
			ErlideErlcPlugin.getResourceString("MakeEnvironmentBlock.0"), ErlideErlcPlugin.getResourceString("MakeEnvironmentBlock.1") }; //$NON-NLS-1$ //$NON-NLS-2$

	protected ColumnLayoutData[] envTableColumnLayouts = {
			new ColumnPixelData(150), new ColumnPixelData(250) };

	private static final String NAME_LABEL = ErlideErlcPlugin
			.getResourceString("MakeEnvironmentBlock.2"); //$NON-NLS-1$

	private static final String VALUE_LABEL = ErlideErlcPlugin
			.getResourceString("MakeEnvironmentBlock.3"); //$NON-NLS-1$

	protected static final String P_VARIABLE = "variable"; //$NON-NLS-1$

	protected static final String P_VALUE = "value"; //$NON-NLS-1$

	protected static String[] envTableColumnProperties = { P_VARIABLE, P_VALUE };

	protected Button envAddButton;

	protected Button envEditButton;

	protected Button envRemoveButton;

	protected Button appendEnvironment;

	protected Button replaceEnvironment;

	protected Button envSelectButton;

	static class EnvironmentVariable {

		// The name of the environment variable
		private String name;

		// The value of the environment variable
		private String value;

		EnvironmentVariable(String name, String value) {
			this.name = name;
			this.value = value;
		}

		/**
		 * Returns this variable's name, which serves as the key in the
		 * key/value pair this variable represents
		 * 
		 * @return this variable's name
		 */
		public String getName() {
			return name;
		}

		/**
		 * Returns this variables value.
		 * 
		 * @return this variable's value
		 */
		public String getValue() {
			return value;
		}

		/**
		 * Sets this variable's value
		 * 
		 * @param value
		 */
		public void setValue(String value) {
			this.value = value;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			return getName();
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		@Override
		public boolean equals(Object obj) {
			boolean equal = false;
			if (obj instanceof EnvironmentVariable) {
				final EnvironmentVariable var = (EnvironmentVariable) obj;
				equal = var.getName().equals(name);
			}
			return equal;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			return name.hashCode();
		}
	}

	/**
	 * Content provider for the environment table
	 */
	protected class EnvironmentVariableContentProvider implements
			IStructuredContentProvider {

		@SuppressWarnings("unchecked")
		public Object[] getElements(Object inputElement) {
			EnvironmentVariable[] elements = new EnvironmentVariable[0];
			final IMakeCommonBuildInfo info = (IMakeCommonBuildInfo) inputElement;
			final Map<String, String> m = info.getEnvironment();
			if (m != null && !m.isEmpty()) {
				elements = new EnvironmentVariable[m.size()];
				final String[] varNames = new String[m.size()];
				m.keySet().toArray(varNames);
				for (int i = 0; i < m.size(); i++) {
					elements[i] = new EnvironmentVariable(varNames[i], m
							.get(varNames[i]));
				}
			}
			return elements;
		}

		public void dispose() {
		}

		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			if (newInput == null) {
				return;
			}
			if (viewer instanceof TableViewer) {
				final TableViewer tableViewer = (TableViewer) viewer;
				if (tableViewer.getTable().isDisposed()) {
					return;
				}
				tableViewer.setSorter(new ViewerSorter() {

					@Override
					public int compare(Viewer iviewer, Object e1, Object e2) {
						if (e1 == null) {
							return -1;
						} else if (e2 == null) {
							return 1;
						} else {
							return ((EnvironmentVariable) e1).getName()
									.compareToIgnoreCase(
											((EnvironmentVariable) e2)
													.getName());
						}
					}
				});
			}
		}
	}

	/**
	 * Label provider for the environment table
	 */
	public static class EnvironmentVariableLabelProvider extends LabelProvider
			implements ITableLabelProvider {

		public String getColumnText(Object element, int columnIndex) {
			String result = null;
			if (element != null) {
				final EnvironmentVariable var = (EnvironmentVariable) element;
				switch (columnIndex) {
				case 0: // variable
					result = var.getName();
					break;
				case 1: // value
					result = var.getValue();
					break;
				}
			}
			return result;
		}

		public Image getColumnImage(Object element, int columnIndex) {
			if (columnIndex == 0) {
				// FIXME image return
				// MakeUIImages.getImage(MakeUIImages.IMG_OBJS_ENV_VAR);
			}
			return null;
		}
	}

	public MakeEnvironmentBlock(Preferences prefs, String builderID) {
		super(ErlideErlcPlugin.getResourceString("MakeEnvironmentBlock.4")); //$NON-NLS-1$
		setDescription(ErlideErlcPlugin
				.getResourceString("MakeEnvironmentBlock.5")); //$NON-NLS-1$
		fPrefs = prefs;
		fBuilderID = builderID;
	}

	@Override
	public void setContainer(IErlOptionContainer container) {
		super.setContainer(container);
		if (getContainer().getProject() != null) {
			try {
				fBuildInfo = ErlideErlcPlugin.createBuildInfo(getContainer()
						.getProject(), fBuilderID);
			} catch (final CoreException e) {
			}
		} else {
			fBuildInfo = ErlideErlcPlugin.createBuildInfo(fPrefs, fBuilderID,
					false);
		}
	}

	@Override
	public void performApply(IProgressMonitor monitor) throws CoreException {
		// Missing builder info
		if (fBuildInfo == null) {
			return;
		}
		if (monitor == null) {
			monitor = new NullProgressMonitor();
		}
		final IWorkspace workspace = ResourcesPlugin.getWorkspace();
		// To avoid multi-build
		final IWorkspaceRunnable operation = new IWorkspaceRunnable() {

			public void run(IProgressMonitor amonitor) throws CoreException {
				amonitor
						.beginTask(
								ErlideErlcPlugin
										.getResourceString("SettingsBlock.monitor.applyingSettings"), 1); //$NON-NLS-1$
				IMakeCommonBuildInfo info = null;
				if (getContainer().getProject() != null) {
					try {
						info = ErlideErlcPlugin.createBuildInfo(getContainer()
								.getProject(), fBuilderID);
					} catch (CoreException e) {
						// disabled builder... just log it
						ErlangPlugin.log(e);
						return;
					}
				} else {
					info = ErlideErlcPlugin.createBuildInfo(fPrefs, fBuilderID,
							false);
				}
				// Convert the table's items into a Map so that this can be
				// saved in the
				// configuration's attributes.
				TableItem[] items = environmentTable.getTable().getItems();
				Map<String, String> map = new HashMap<String, String>(
						items.length);
				for (TableItem element : items) {
					EnvironmentVariable var = (EnvironmentVariable) element
							.getData();
					map.put(var.getName(), var.getValue());
				}
				info.setEnvironment(map);
				info.setAppendEnvironment(appendEnvironment.getSelection());
			}
		};
		if (getContainer().getProject() != null) {
			workspace.run(operation, monitor);
		} else {
			operation.run(monitor);
		}
	}

	/**
	 * Updates the environment table for the given launch configuration
	 * 
	 * @param configuration
	 */
	protected void updateEnvironment(IMakeCommonBuildInfo info) {
		environmentTable.setInput(info);
	}

	@Override
	public void performDefaults() {
		// Missing builder info
		if (fBuildInfo == null) {
			return;
		}

		IMakeCommonBuildInfo info;
		if (getContainer().getProject() != null) {
			info = ErlideErlcPlugin.createBuildInfo(fPrefs, fBuilderID, false);
		} else {
			info = ErlideErlcPlugin.createBuildInfo(fPrefs, fBuilderID, true);
		}
		final boolean append = info.appendEnvironment();
		if (append) {
			appendEnvironment.setSelection(true);
			replaceEnvironment.setSelection(false);
		} else {
			replaceEnvironment.setSelection(true);
			appendEnvironment.setSelection(false);
		}
		updateEnvironment(info);
		updateAppendReplace();
	}

	@Override
	public void createControl(Composite parent) {
		final Composite composite = ControlFactory.createComposite(parent, 1);
		setControl(composite);

		// FIXME help system
		// ErlideErlcPlugin.getDefault().getWorkbench().getHelpSystem().setHelp(getControl(),
		// IMakeHelpContextIds.MAKE_BUILDER_SETTINGS);

		if (fBuildInfo == null) {
			ControlFactory.createEmptySpace(composite);
			ControlFactory
					.createLabel(
							composite,
							ErlideErlcPlugin
									.getResourceString("SettingsBlock.label.missingBuilderInformation")); //$NON-NLS-1$
			return;
		}

		final GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		final GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
		composite.setLayout(layout);
		composite.setLayoutData(gridData);
		composite.setFont(parent.getFont());

		createBuildEnvironmentControls(composite);
		createTableButtons(composite);
		createAppendReplace(composite);

		final boolean append = fBuildInfo.appendEnvironment();
		if (append) {
			appendEnvironment.setSelection(true);
			replaceEnvironment.setSelection(false);
		} else {
			replaceEnvironment.setSelection(true);
			appendEnvironment.setSelection(false);
		}
		updateEnvironment(fBuildInfo);
		updateAppendReplace();

	}

	private void createBuildEnvironmentControls(Composite parent) {
		final Font font = parent.getFont();
		// Create table composite
		final Composite tableComposite = new Composite(parent, SWT.NONE);
		final GridLayout layout = new GridLayout();
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		layout.numColumns = 1;
		GridData gridData = new GridData(GridData.FILL_BOTH);
		gridData.heightHint = 150;
		tableComposite.setLayout(layout);
		tableComposite.setLayoutData(gridData);
		tableComposite.setFont(font);
		// Create label
		final Label label = new Label(tableComposite, SWT.NONE);
		label.setFont(font);
		label.setText(ErlideErlcPlugin
				.getResourceString("MakeEnvironmentBlock.6")); //$NON-NLS-1$
		// Create table
		environmentTable = new TableViewer(tableComposite, SWT.BORDER
				| SWT.H_SCROLL | SWT.V_SCROLL | SWT.MULTI | SWT.FULL_SELECTION);
		final Table table = environmentTable.getTable();
		final TableLayout tableLayout = new TableLayout();
		table.setLayout(tableLayout);
		table.setHeaderVisible(true);
		table.setFont(font);
		gridData = new GridData(GridData.FILL_BOTH);
		environmentTable.getControl().setLayoutData(gridData);
		environmentTable
				.setContentProvider(new EnvironmentVariableContentProvider());
		environmentTable
				.setLabelProvider(new EnvironmentVariableLabelProvider());
		environmentTable.setColumnProperties(envTableColumnProperties);
		environmentTable
				.addSelectionChangedListener(new ISelectionChangedListener() {

					public void selectionChanged(SelectionChangedEvent event) {
						handleTableSelectionChanged(event);
					}
				});
		environmentTable.addDoubleClickListener(new IDoubleClickListener() {

			public void doubleClick(DoubleClickEvent event) {
				if (!environmentTable.getSelection().isEmpty()) {
					handleEnvEditButtonSelected();
				}
			}
		});
		// Create columns
		for (int i = 0; i < envTableColumnHeaders.length; i++) {
			tableLayout.addColumnData(envTableColumnLayouts[i]);
			final TableColumn tc = new TableColumn(table, SWT.NONE, i);
			tc.setResizable(envTableColumnLayouts[i].resizable);
			tc.setText(envTableColumnHeaders[i]);
		}
	}

	/**
	 * Responds to a selection changed event in the environment table
	 * 
	 * @param event
	 *            the selection change event
	 */
	protected void handleTableSelectionChanged(SelectionChangedEvent event) {
		final int size = ((IStructuredSelection) event.getSelection()).size();
		envEditButton.setEnabled(size == 1);
		envRemoveButton.setEnabled(size > 0);
	}

	/**
	 * Create some empty space.
	 */
	protected void createVerticalSpacer(Composite comp, int colSpan) {
		final Label label = new Label(comp, SWT.NONE);
		final GridData gd = new GridData();
		gd.horizontalSpan = colSpan;
		label.setLayoutData(gd);
		label.setFont(comp.getFont());
	}

	/**
	 * Creates the add/edit/remove buttons for the environment table
	 * 
	 * @param parent
	 *            the composite in which the buttons should be created
	 */
	protected void createTableButtons(Composite parent) {
		// Create button composite
		final Composite buttonComposite = new Composite(parent, SWT.NONE);
		final GridLayout glayout = new GridLayout();
		glayout.marginHeight = 0;
		glayout.marginWidth = 0;
		glayout.numColumns = 1;
		final GridData gdata = new GridData(GridData.VERTICAL_ALIGN_BEGINNING
				| GridData.HORIZONTAL_ALIGN_END);
		buttonComposite.setLayout(glayout);
		buttonComposite.setLayoutData(gdata);
		buttonComposite.setFont(parent.getFont());

		createVerticalSpacer(buttonComposite, 1);
		// Create buttons
		envAddButton = createPushButton(buttonComposite, ErlideErlcPlugin
				.getResourceString("MakeEnvironmentBlock.7"), null); //$NON-NLS-1$
		envAddButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent event) {
				handleEnvAddButtonSelected();
			}
		});
		envSelectButton = createPushButton(buttonComposite, ErlideErlcPlugin
				.getResourceString("MakeEnvironmentBlock.8"), null); //$NON-NLS-1$
		envSelectButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent event) {
				handleEnvSelectButtonSelected();
			}
		});
		envEditButton = createPushButton(buttonComposite, ErlideErlcPlugin
				.getResourceString("MakeEnvironmentBlock.9"), null); //$NON-NLS-1$
		envEditButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent event) {
				handleEnvEditButtonSelected();
			}
		});
		envEditButton.setEnabled(false);
		envRemoveButton = createPushButton(buttonComposite, ErlideErlcPlugin
				.getResourceString("MakeEnvironmentBlock.10"), null); //$NON-NLS-1$
		envRemoveButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent event) {
				handleEnvRemoveButtonSelected();
			}
		});
		envRemoveButton.setEnabled(false);
	}

	/**
	 * Adds a new environment variable to the table.
	 */
	protected void handleEnvAddButtonSelected() {
		final MultipleInputDialog dialog = new MultipleInputDialog(getShell(),
				ErlideErlcPlugin.getResourceString("MakeEnvironmentBlock.11")); //$NON-NLS-1$
		dialog.addTextField(NAME_LABEL, null, false);
		dialog.addVariablesField(VALUE_LABEL, null, true);

		if (dialog.open() != Window.OK) {
			return;
		}

		final String name = dialog.getStringValue(NAME_LABEL);
		final String value = dialog.getStringValue(VALUE_LABEL);

		if (name != null && value != null && name.length() > 0
				&& value.length() > 0) {
			addVariable(new EnvironmentVariable(name.trim(), value.trim()));
			updateAppendReplace();
		}
	}

	/**
	 * Updates the enablement of the append/replace widgets. The widgets should
	 * disable when there are no environment variables specified.
	 */
	protected void updateAppendReplace() {
		final boolean enable = environmentTable.getTable().getItemCount() > 0;
		appendEnvironment.setEnabled(enable);
		replaceEnvironment.setEnabled(enable);
	}

	/**
	 * Attempts to add the given variable. Returns whether the variable was
	 * added or not (as when the user answers not to overwrite an existing
	 * variable).
	 * 
	 * @param variable
	 *            the variable to add
	 * @return whether the variable was added
	 */
	protected boolean addVariable(EnvironmentVariable variable) {
		final String name = variable.getName();
		final TableItem[] items = environmentTable.getTable().getItems();
		for (TableItem element : items) {
			final EnvironmentVariable existingVariable = (EnvironmentVariable) element
					.getData();
			if (name.equals(existingVariable.getName())) {
				boolean overWrite = MessageDialog
						.openQuestion(
								getShell(),
								ErlideErlcPlugin
										.getResourceString("MakeEnvironmentBlock.12"), MessageFormat.format( //$NON-NLS-1$
												ErlideErlcPlugin
														.getResourceString("MakeEnvironmentBlock.13"), (Object[]) new String[] { name })); //$NON-NLS-1$
				if (!overWrite) {
					return false;
				}
				environmentTable.remove(existingVariable);
				break;
			}
		}
		environmentTable.add(variable);
		getContainer().updateContainer();
		return true;
	}

	/**
	 * Gets native environment variable. Creates EnvironmentVariable objects.
	 * 
	 * @return Map of name - EnvironmentVariable pairs based on native
	 *         environment.
	 */
	private Map<Object, Object> getNativeEnvironment() {
		final Map<Object, Object> stringVars = EnvironmentReader.getEnvVars();
		final HashMap<Object, Object> vars = new HashMap<Object, Object>();
		for (Object element : stringVars.keySet()) {
			final String key = (String) element;
			final String value = (String) stringVars.get(key);
			vars.put(key, new EnvironmentVariable(key, value));
		}
		return vars;
	}

	/**
	 * Displays a dialog that allows user to select native environment variables
	 * to add to the table.
	 */
	protected void handleEnvSelectButtonSelected() {
		// get Environment Variables from the OS
		final Map<Object, Object> envVariables = getNativeEnvironment();

		// get Environment Variables from the table
		final TableItem[] items = environmentTable.getTable().getItems();
		for (TableItem element : items) {
			final EnvironmentVariable var = (EnvironmentVariable) element
					.getData();
			envVariables.remove(var.getName());
		}

		final ListSelectionDialog dialog = new NativeEnvironmentDialog(
				getShell(), envVariables,
				createSelectionDialogContentProvider(),
				createSelectionDialogLabelProvider(), ErlideErlcPlugin
						.getResourceString("MakeEnvironmentBlock.14")); //$NON-NLS-1$
		dialog.setTitle(ErlideErlcPlugin
				.getResourceString("MakeEnvironmentBlock.15")); //$NON-NLS-1$

		final int button = dialog.open();
		if (button == Window.OK) {
			final Object[] selected = dialog.getResult();
			for (Object element : selected) {
				environmentTable.add(element);
			}
		}

		updateAppendReplace();
		getContainer().updateContainer();
	}

	/**
	 * Creates a label provider for the native native environment variable
	 * selection dialog.
	 * 
	 * @return A label provider for the native native environment variable
	 *         selection dialog.
	 */
	private ILabelProvider createSelectionDialogLabelProvider() {
		return new ILabelProvider() {

			public Image getImage(Object element) {
				// FIXME images return
				// MakeUIImages.getImage(MakeUIImages.IMG_OBJS_ENVIRONMNET);
				return null;
			}

			public String getText(Object element) {
				final EnvironmentVariable var = (EnvironmentVariable) element;
				return var.getName() + " [" + var.getValue() + "]"; //$NON-NLS-1$ //$NON-NLS-2$
			}

			public void addListener(ILabelProviderListener listener) {
			}

			public void dispose() {
			}

			public boolean isLabelProperty(Object element, String property) {
				return false;
			}

			public void removeListener(ILabelProviderListener listener) {
			}
		};
	}

	/**
	 * Creates a content provider for the native native environment variable
	 * selection dialog.
	 * 
	 * @return A content provider for the native native environment variable
	 *         selection dialog.
	 */
	private IStructuredContentProvider createSelectionDialogContentProvider() {
		return new IStructuredContentProvider() {

			@SuppressWarnings("unchecked")
			public Object[] getElements(Object inputElement) {
				EnvironmentVariable[] elements = null;
				if (inputElement instanceof Map) {
					final Comparator<String> comparator = new Comparator<String>() {
						public int compare(String o1, String o2) {
							return o1.compareTo(o2);
						}
					};
					final TreeMap<String, Object> envVars = new TreeMap<String, Object>(
							comparator);
					envVars.putAll((Map<String, Object>) inputElement);
					elements = new EnvironmentVariable[envVars.size()];
					int index = 0;
					for (Object key : envVars.keySet()) {
						elements[index] = (EnvironmentVariable) envVars
								.get(key);
					}
				}
				return elements;
			}

			public void dispose() {
			}

			public void inputChanged(Viewer viewer, Object oldInput,
					Object newInput) {
			}
		};
	}

	/**
	 * Creates an editor for the value of the selected environment variable.
	 */
	protected void handleEnvEditButtonSelected() {
		final IStructuredSelection sel = (IStructuredSelection) environmentTable
				.getSelection();
		final EnvironmentVariable var = (EnvironmentVariable) sel
				.getFirstElement();
		if (var == null) {
			return;
		}
		final String originalName = var.getName();
		String value = var.getValue();
		final MultipleInputDialog dialog = new MultipleInputDialog(getShell(),
				ErlideErlcPlugin.getResourceString("MakeEnvironmentBlock.16")); //$NON-NLS-1$
		dialog.addTextField(NAME_LABEL, originalName, false);
		dialog.addVariablesField(VALUE_LABEL, value, true);

		if (dialog.open() != Window.OK) {
			return;
		}
		final String name = dialog.getStringValue(NAME_LABEL);
		value = dialog.getStringValue(VALUE_LABEL);
		if (!originalName.equals(name)) {
			if (addVariable(new EnvironmentVariable(name, value))) {
				environmentTable.remove(var);
			}
		} else {
			var.setValue(value);
			environmentTable.update(var, null);
			getContainer().updateContainer();
		}
	}

	/**
	 * Removes the selected environment variable from the table.
	 */
	protected void handleEnvRemoveButtonSelected() {
		final IStructuredSelection sel = (IStructuredSelection) environmentTable
				.getSelection();
		environmentTable.getControl().setRedraw(false);
		for (final Iterator<?> i = sel.iterator(); i.hasNext();) {
			final EnvironmentVariable var = (EnvironmentVariable) i.next();
			environmentTable.remove(var);
		}
		environmentTable.getControl().setRedraw(true);
		updateAppendReplace();
		getContainer().updateContainer();
	}

	private static class NativeEnvironmentDialog extends ListSelectionDialog {

		public NativeEnvironmentDialog(Shell parentShell, Object input,
				IStructuredContentProvider contentProvider,
				ILabelProvider labelProvider, String message) {
			super(parentShell, input, contentProvider, labelProvider, message);
			setShellStyle(getShellStyle() | SWT.RESIZE);
		}

		protected IDialogSettings getDialogSettings() {
			final IDialogSettings settings = ErlideErlcPlugin.getDefault()
					.getDialogSettings();
			IDialogSettings section = settings
					.getSection(getDialogSettingsSectionName());
			if (section == null) {
				section = settings
						.addNewSection(getDialogSettingsSectionName());
			}
			return section;
		}

		/**
		 * Returns the name of the section that this dialog stores its settings
		 * in
		 * 
		 * @return String
		 */
		protected String getDialogSettingsSectionName() {
			return ErlideErlcPlugin.PLUGIN_ID
					+ ".ENVIRONMENT_TAB.NATIVE_ENVIROMENT_DIALOG"; //$NON-NLS-1$
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.jface.window.Window#getInitialLocation(org.eclipse.swt.graphics.Point)
		 */
		@Override
		protected Point getInitialLocation(Point initialSize) {
			final Point initialLocation = DialogSettingsHelper
					.getInitialLocation(getDialogSettingsSectionName());
			if (initialLocation != null) {
				return initialLocation;
			}
			return super.getInitialLocation(initialSize);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.jface.window.Window#getInitialSize()
		 */
		@Override
		protected Point getInitialSize() {
			final Point size = super.getInitialSize();
			return DialogSettingsHelper.getInitialSize(
					getDialogSettingsSectionName(), size);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.jface.window.Window#close()
		 */
		@Override
		public boolean close() {
			DialogSettingsHelper.persistShellGeometry(getShell(),
					getDialogSettingsSectionName());
			return super.close();
		}
	}

	/**
	 * Creates and configures the widgets which allow the user to choose whether
	 * the specified environment should be appended to the native environment or
	 * if it should completely replace it.
	 * 
	 * @param parent
	 *            the composite in which the widgets should be created
	 */
	protected void createAppendReplace(Composite parent) {
		final Composite appendReplaceComposite = new Composite(parent, SWT.NONE);
		final GridData gridData = new GridData();
		gridData.horizontalSpan = 2;
		final GridLayout layout = new GridLayout();
		appendReplaceComposite.setLayoutData(gridData);
		appendReplaceComposite.setLayout(layout);
		appendReplaceComposite.setFont(parent.getFont());

		appendEnvironment = createRadioButton(appendReplaceComposite,
				ErlideErlcPlugin.getResourceString("MakeEnvironmentBlock.17")); //$NON-NLS-1$
		appendEnvironment.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				getContainer().updateContainer();
			}
		});
		replaceEnvironment = createRadioButton(appendReplaceComposite,
				ErlideErlcPlugin.getResourceString("MakeEnvironmentBlock.18")); //$NON-NLS-1$
	}

}
