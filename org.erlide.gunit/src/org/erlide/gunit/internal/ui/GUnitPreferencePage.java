/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Sebastian Davids: sdavids@gmx.de
 *******************************************************************************/
package org.erlide.gunit.internal.ui;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ColumnWeightData;
import org.eclipse.jface.viewers.ContentViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TableEditor;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.PlatformUI;
import org.erlide.gunit.internal.util.LayoutUtil;
import org.erlide.ui.ErlideUIConstants;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.internal.util.TableLayoutComposite;
import org.erlide.ui.util.SWTUtil;

/**
 * Preference page for JUnit settings. Supports to define the failure stack
 * filter patterns.
 */
public class GUnitPreferencePage extends PreferencePage implements
IWorkbenchPreferencePage {

	private static final String DEFAULT_NEW_FILTER_TEXT = ""; //$NON-NLS-1$

	private static final Image IMG_CUNIT = ErlideUIPlugin.getDefault()
	.getImage(ErlideUIConstants.IMG_FOLDER_LABEL);

	private static final Image IMG_PKG = ErlideUIPlugin.getDefault().getImage(
			ISharedImages.IMG_OBJ_FOLDER);

	// Step filter widgets
	private Label fFilterViewerLabel;

	private CheckboxTableViewer fFilterViewer;

	private Table fFilterTable;

	private Button fAddPackageButton;

	private Button fAddTypeButton;

	private Button fRemoveFilterButton;

	private Button fAddFilterButton;

	private Button fEnableAllButton;

	private Button fDisableAllButton;

	private Text fEditorText;

	private String fInvalidEditorText = null;

	private TableEditor fTableEditor;

	private TableItem fNewTableItem;

	private Filter fNewStackFilter;

	private StackFilterContentProvider fStackFilterContentProvider;

	/**
	 * Model object that represents a single entry in the filter table.
	 */
	private static class Filter {

		private String fName;

		private boolean fChecked;

		public Filter(final String name, final boolean checked) {
			setName(name);
			setChecked(checked);
		}

		public String getName() {
			return this.fName;
		}

		public void setName(final String name) {
			this.fName = name;
		}

		public boolean isChecked() {
			return this.fChecked;
		}

		public void setChecked(final boolean checked) {
			this.fChecked = checked;
		}

		@Override
		public boolean equals(final Object o) {
			if (!(o instanceof Filter)) {
				return false;
			}

			final Filter other = (Filter) o;
			return (getName().equals(other.getName()));
		}

		@Override
		public int hashCode() {
			return this.fName.hashCode();
		}
	}

	/**
	 * Sorter for the filter table; sorts alphabetically ascending.
	 */
	private static class FilterViewerSorter extends ViewerComparator {
		@SuppressWarnings("unchecked")
		@Override
		public int compare(final Viewer viewer, final Object e1, final Object e2) {
			final ILabelProvider lprov = (ILabelProvider) ((ContentViewer) viewer)
			.getLabelProvider();
			String name1 = lprov.getText(e1);
			String name2 = lprov.getText(e2);
			if (name1 == null) {
				name1 = "";
			}

			if (name2 == null) {
				name2 = "";
			}

			if (name1.length() > 0 && name2.length() > 0) {
				final char char1 = name1.charAt(name1.length() - 1);
				final char char2 = name2.charAt(name2.length() - 1);
				if (char1 == '*' && char1 != char2) {
					return -1;
				}

				if (char2 == '*' && char2 != char1) {
					return 1;
				}
			}
			return getComparator().compare(name1, name2);
		}
	}

	/**
	 * Label provider for Filter model objects
	 */
	private static class FilterLabelProvider extends LabelProvider implements
	ITableLabelProvider {

		public String getColumnText(final Object object, final int column) {
			return (column == 0) ? ((Filter) object).getName() : "";
		}

		@Override
		public String getText(final Object element) {
			return ((Filter) element).getName();
		}

		public Image getColumnImage(final Object object, final int column) {
			final String name = ((Filter) object).getName();
			if (name.endsWith(".*")
					|| name
					.equals(GUnitMessages.JUnitMainTab_label_defaultpackage)) {
				// package
				return IMG_PKG;
			} else if ("".equals(name)) {
				// needed for the in-place editor
				return null;
			} else if ((Character.isUpperCase(name.charAt(0)))
					&& (name.indexOf('.') < 0)) {
				// class in default package
				return IMG_CUNIT;
			} else {
				// fully-qualified class or other filter
				final int lastDotIndex = name.lastIndexOf('.');
				if ((-1 != lastDotIndex)
						&& ((name.length() - 1) != lastDotIndex)
						&& Character.isUpperCase(name.charAt(lastDotIndex + 1))) {
					return IMG_CUNIT;
				}
			}
			// other filter
			return null;
		}
	}

	/**
	 * Content provider for the filter table. Content consists of instances of
	 * Filter.
	 */
	private class StackFilterContentProvider implements
	IStructuredContentProvider {

		private List<Filter> fFilters;

		public StackFilterContentProvider() {
			final List<String> active = createActiveStackFiltersList();
			final List<String> inactive = createInactiveStackFiltersList();
			populateFilters(active, inactive);
		}

		public void setDefaults() {
			GUnitPreferencePage.this.fFilterViewer.remove(this.fFilters
					.toArray());
			final List<String> active = GUnitPreferencesConstants
			.createDefaultStackFiltersList();
			final List<String> inactive = new ArrayList<String>();
			populateFilters(active, inactive);
		}

		protected void populateFilters(final List<String> activeList,
				final List<String> inactiveList) {
			this.fFilters = new ArrayList<Filter>(activeList.size()
					+ inactiveList.size());
			populateList(activeList, true);
			if (inactiveList.size() != 0) {
				populateList(inactiveList, false);
			}
		}

		protected void populateList(final List<String> list, final boolean checked) {
			final Iterator<String> iterator = list.iterator();

			while (iterator.hasNext()) {
				final String name = iterator.next();
				addFilter(name, checked);
			}
		}

		public Filter addFilter(final String name, final boolean checked) {
			final Filter filter = new Filter(name, checked);
			if (!this.fFilters.contains(filter)) {
				this.fFilters.add(filter);
				GUnitPreferencePage.this.fFilterViewer.add(filter);
				GUnitPreferencePage.this.fFilterViewer.setChecked(filter,
						checked);
			}
			updateActions();
			return filter;
		}

		public void saveFilters() {
			final List<String> active = new ArrayList<String>(this.fFilters.size());
			final List<String> inactive = new ArrayList<String>(this.fFilters.size());
			final Iterator<Filter> iterator = this.fFilters.iterator();
			while (iterator.hasNext()) {
				final Filter filter = iterator.next();
				final String name = filter.getName();
				if (filter.isChecked()) {
					active.add(name);
				} else {
					inactive.add(name);
				}
			}
			String pref = GUnitPreferencesConstants.serializeList(active
					.toArray(new String[active.size()]));
			getPreferenceStore().setValue(
					GUnitPreferencesConstants.PREF_ACTIVE_FILTERS_LIST, pref);
			pref = GUnitPreferencesConstants.serializeList(inactive
					.toArray(new String[inactive.size()]));
			getPreferenceStore().setValue(
					GUnitPreferencesConstants.PREF_INACTIVE_FILTERS_LIST, pref);
		}

		public void removeFilters(final Object[] filters) {
			for (int i = (filters.length - 1); i >= 0; --i) {
				final Filter filter = (Filter) filters[i];
				this.fFilters.remove(filter);
			}
			GUnitPreferencePage.this.fFilterViewer.remove(filters);
			updateActions();
		}

		public void toggleFilter(final Filter filter) {
			final boolean newState = !filter.isChecked();
			filter.setChecked(newState);
			GUnitPreferencePage.this.fFilterViewer.setChecked(filter, newState);
		}

		public Object[] getElements(final Object inputElement) {
			return this.fFilters.toArray();
		}

		public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput) {
		}

		public void dispose() {
		}

	}

	public GUnitPreferencePage() {
		super();
		setDescription(GUnitMessages.JUnitPreferencePage_description);
		setPreferenceStore(GUnitPlugin.getDefault().getPreferenceStore());
	}

	@Override
	protected Control createContents(final Composite parent) {
		PlatformUI.getWorkbench().getHelpSystem().setHelp(parent,
				IGUnitHelpContextIds.JUNIT_PREFERENCE_PAGE);

		final Composite composite = new Composite(parent, SWT.NULL);
		final GridLayout layout = new GridLayout();
		layout.numColumns = 1;
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		composite.setLayout(layout);
		final GridData data = new GridData();
		data.verticalAlignment = GridData.FILL;
		data.horizontalAlignment = GridData.FILL;
		composite.setLayoutData(data);

		createStackFilterPreferences(composite);
		Dialog.applyDialogFont(composite);
		return composite;
	}

	/*
	 * Create a group to contain the step filter related widgets
	 */
	private void createStackFilterPreferences(final Composite composite) {
		this.fFilterViewerLabel = new Label(composite, SWT.SINGLE | SWT.LEFT);
		this.fFilterViewerLabel
		.setText(GUnitMessages.JUnitPreferencePage_filter_label);

		final Composite container = new Composite(composite, SWT.NONE);
		final GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		container.setLayout(layout);
		final GridData gd = new GridData(GridData.FILL_BOTH);
		container.setLayoutData(gd);

		createFilterTable(container);
		createStepFilterButtons(container);
	}

	private void createFilterTable(final Composite container) {
		final TableLayoutComposite layouter = new TableLayoutComposite(container,
				SWT.NONE);
		layouter.addColumnData(new ColumnWeightData(100));
		layouter.setLayoutData(new GridData(GridData.FILL_BOTH));

		this.fFilterTable = new Table(layouter, SWT.CHECK | SWT.BORDER
				| SWT.MULTI | SWT.FULL_SELECTION);

		new TableColumn(this.fFilterTable, SWT.NONE);
		this.fFilterViewer = new CheckboxTableViewer(this.fFilterTable);
		this.fTableEditor = new TableEditor(this.fFilterTable);
		this.fFilterViewer.setLabelProvider(new FilterLabelProvider());
		this.fFilterViewer.setComparator(new FilterViewerSorter());
		this.fStackFilterContentProvider = new StackFilterContentProvider();
		this.fFilterViewer.setContentProvider(this.fStackFilterContentProvider);
		// input just needs to be non-null
		this.fFilterViewer.setInput(this);
		this.fFilterViewer.addCheckStateListener(new ICheckStateListener() {
			public void checkStateChanged(final CheckStateChangedEvent event) {
				final Filter filter = (Filter) event.getElement();
				GUnitPreferencePage.this.fStackFilterContentProvider
				.toggleFilter(filter);
			}
		});
		this.fFilterViewer
		.addSelectionChangedListener(new ISelectionChangedListener() {
			public void selectionChanged(final SelectionChangedEvent event) {
				final ISelection selection = event.getSelection();
				GUnitPreferencePage.this.fRemoveFilterButton
				.setEnabled(!selection.isEmpty());
			}
		});
	}

	private void createStepFilterButtons(final Composite container) {
		final Composite buttonContainer = new Composite(container, SWT.NONE);
		GridData gd = new GridData(GridData.FILL_VERTICAL);
		buttonContainer.setLayoutData(gd);
		final GridLayout buttonLayout = new GridLayout();
		buttonLayout.numColumns = 1;
		buttonLayout.marginHeight = 0;
		buttonLayout.marginWidth = 0;
		buttonContainer.setLayout(buttonLayout);

		this.fAddFilterButton = new Button(buttonContainer, SWT.PUSH);
		this.fAddFilterButton
		.setText(GUnitMessages.JUnitPreferencePage_addfilterbutton_label);
		this.fAddFilterButton
		.setToolTipText(GUnitMessages.JUnitPreferencePage_addfilterbutton_tooltip);
		gd = new GridData(GridData.FILL_HORIZONTAL
				| GridData.VERTICAL_ALIGN_BEGINNING);
		this.fAddFilterButton.setLayoutData(gd);
		LayoutUtil.setButtonDimensionHint(this.fAddFilterButton);
		this.fAddFilterButton.addListener(SWT.Selection, new Listener() {
			public void handleEvent(final Event e) {
				editFilter();
			}
		});

		this.fAddTypeButton = new Button(buttonContainer, SWT.PUSH);
		this.fAddTypeButton
		.setText(GUnitMessages.JUnitPreferencePage_addtypebutton_label);
		this.fAddTypeButton
		.setToolTipText(GUnitMessages.JUnitPreferencePage_addtypebutton_tooltip);
		gd = getButtonGridData(this.fAddTypeButton);
		this.fAddTypeButton.setLayoutData(gd);
		LayoutUtil.setButtonDimensionHint(this.fAddTypeButton);
		this.fAddTypeButton.addListener(SWT.Selection, new Listener() {
			public void handleEvent(final Event e) {
				addType();
			}
		});

		this.fAddPackageButton = new Button(buttonContainer, SWT.PUSH);
		this.fAddPackageButton
		.setText(GUnitMessages.JUnitPreferencePage_addpackagebutton_label);
		this.fAddPackageButton
		.setToolTipText(GUnitMessages.JUnitPreferencePage_addpackagebutton_tooltip);
		gd = getButtonGridData(this.fAddPackageButton);
		this.fAddPackageButton.setLayoutData(gd);
		SWTUtil.setButtonDimensionHint(this.fAddPackageButton);
		this.fAddPackageButton.addListener(SWT.Selection, new Listener() {
			public void handleEvent(final Event e) {
				addPackage();
			}
		});

		this.fRemoveFilterButton = new Button(buttonContainer, SWT.PUSH);
		this.fRemoveFilterButton
		.setText(GUnitMessages.JUnitPreferencePage_removefilterbutton_label);
		this.fRemoveFilterButton
		.setToolTipText(GUnitMessages.JUnitPreferencePage_removefilterbutton_tooltip);
		gd = getButtonGridData(this.fRemoveFilterButton);
		this.fRemoveFilterButton.setLayoutData(gd);
		SWTUtil.setButtonDimensionHint(this.fRemoveFilterButton);
		this.fRemoveFilterButton.addListener(SWT.Selection, new Listener() {
			public void handleEvent(final Event e) {
				removeFilters();
			}
		});
		this.fRemoveFilterButton.setEnabled(false);

		this.fEnableAllButton = new Button(buttonContainer, SWT.PUSH);
		this.fEnableAllButton
		.setText(GUnitMessages.JUnitPreferencePage_enableallbutton_label);
		this.fEnableAllButton
		.setToolTipText(GUnitMessages.JUnitPreferencePage_enableallbutton_tooltip);
		gd = getButtonGridData(this.fEnableAllButton);
		this.fEnableAllButton.setLayoutData(gd);
		SWTUtil.setButtonDimensionHint(this.fEnableAllButton);
		this.fEnableAllButton.addListener(SWT.Selection, new Listener() {
			public void handleEvent(final Event e) {
				checkAllFilters(true);
			}
		});

		this.fDisableAllButton = new Button(buttonContainer, SWT.PUSH);
		this.fDisableAllButton
		.setText(GUnitMessages.JUnitPreferencePage_disableallbutton_label);
		this.fDisableAllButton
		.setToolTipText(GUnitMessages.JUnitPreferencePage_disableallbutton_tooltip);
		gd = getButtonGridData(this.fDisableAllButton);
		this.fDisableAllButton.setLayoutData(gd);
		SWTUtil.setButtonDimensionHint(this.fDisableAllButton);
		this.fDisableAllButton.addListener(SWT.Selection, new Listener() {
			public void handleEvent(final Event e) {
				checkAllFilters(false);
			}
		});

	}

	private GridData getButtonGridData(final Button button) {
		final GridData gd = new GridData(GridData.FILL_HORIZONTAL
				| GridData.VERTICAL_ALIGN_BEGINNING);
		final int widthHint = convertHorizontalDLUsToPixels(IDialogConstants.BUTTON_WIDTH);
		gd.widthHint = Math.max(widthHint, button.computeSize(SWT.DEFAULT,
				SWT.DEFAULT, true).x);
		return gd;
	}

	public void init(final IWorkbench workbench) {
	}

	/**
	 * Create a new filter in the table (with the default 'new filter' value),
	 * then open up an in-place editor on it.
	 */
	private void editFilter() {
		// if a previous edit is still in progress, finish it
		if (this.fEditorText != null) {
			validateChangeAndCleanup();
		}

		this.fNewStackFilter = this.fStackFilterContentProvider.addFilter(
				DEFAULT_NEW_FILTER_TEXT, true);
		this.fNewTableItem = this.fFilterTable.getItem(0);

		// create & configure Text widget for editor
		// Fix for bug 1766. Border behavior on for text fields varies per
		// platform.
		// On Motif, you always get a border, on other platforms,
		// you don't. Specifying a border on Motif results in the characters
		// getting pushed down so that only there very tops are visible. Thus,
		// we have to specify different style constants for the different
		// platforms.
		int textStyles = SWT.SINGLE | SWT.LEFT;
		if (!SWT.getPlatform().equals("motif")) {
			textStyles |= SWT.BORDER;
		}

		this.fEditorText = new Text(this.fFilterTable, textStyles);
		final GridData gd = new GridData(GridData.FILL_BOTH);
		this.fEditorText.setLayoutData(gd);

		// set the editor
		this.fTableEditor.horizontalAlignment = SWT.LEFT;
		this.fTableEditor.grabHorizontal = true;
		this.fTableEditor.setEditor(this.fEditorText, this.fNewTableItem, 0);

		// get the editor ready to use
		this.fEditorText.setText(this.fNewStackFilter.getName());
		this.fEditorText.selectAll();
		setEditorListeners(this.fEditorText);
		this.fEditorText.setFocus();
	}

	private void setEditorListeners(final Text text) {
		// CR means commit the changes, ESC means abort and don't commit
		text.addKeyListener(new KeyAdapter() {
			@Override
			public void keyReleased(final KeyEvent event) {
				if (event.character == SWT.CR) {
					if (GUnitPreferencePage.this.fInvalidEditorText != null) {
						GUnitPreferencePage.this.fEditorText
						.setText(GUnitPreferencePage.this.fInvalidEditorText);
						GUnitPreferencePage.this.fInvalidEditorText = null;
					} else {
						validateChangeAndCleanup();
					}
				} else if (event.character == SWT.ESC) {
					removeNewFilter();
					cleanupEditor();
				}
			}
		});
		// Consider loss of focus on the editor to mean the same as CR
		text.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(final FocusEvent event) {
				if (GUnitPreferencePage.this.fInvalidEditorText != null) {
					GUnitPreferencePage.this.fEditorText
					.setText(GUnitPreferencePage.this.fInvalidEditorText);
					GUnitPreferencePage.this.fInvalidEditorText = null;
				} else {
					validateChangeAndCleanup();
				}
			}
		});
		// Consume traversal events from the text widget so that CR doesn't
		// traverse away to dialog's default button. Without this, hitting
		// CR in the text field closes the entire dialog.
		text.addListener(SWT.Traverse, new Listener() {
			public void handleEvent(final Event event) {
				event.doit = false;
			}
		});
	}

	private void validateChangeAndCleanup() {
		final String trimmedValue = this.fEditorText.getText().trim();
		// if the new value is blank, remove the filter
		if (trimmedValue.length() < 1) {
			removeNewFilter();
		} else if (!validateEditorInput(trimmedValue)) {
			this.fInvalidEditorText = trimmedValue;
			this.fEditorText
			.setText(GUnitMessages.JUnitPreferencePage_invalidstepfilterreturnescape);
			getShell().getDisplay().beep();
			return;
			// otherwise, commit the new value if not a duplicate
		} else {
			final Object[] filters = this.fStackFilterContentProvider
			.getElements(null);
			for (int i = 0; i < filters.length; i++) {
				final Filter filter = (Filter) filters[i];
				if (filter.getName().equals(trimmedValue)) {
					removeNewFilter();
					cleanupEditor();
					return;
				}
			}
			this.fNewTableItem.setText(trimmedValue);
			this.fNewStackFilter.setName(trimmedValue);
			this.fFilterViewer.refresh();
		}
		cleanupEditor();
	}

	/*
	 * Cleanup all widgets & resources used by the in-place editing
	 */
	private void cleanupEditor() {
		if (this.fEditorText == null) {
			return;
		}

		this.fNewStackFilter = null;
		this.fNewTableItem = null;
		this.fTableEditor.setEditor(null, null, 0);
		this.fEditorText.dispose();
		this.fEditorText = null;
	}

	private void removeNewFilter() {
		this.fStackFilterContentProvider
		.removeFilters(new Object[] { this.fNewStackFilter });
	}

	/*
	 * A valid step filter is simply one that is a valid Java identifier. and,
	 * as defined in the JDI spec, the regular expressions used for step
	 * filtering must be limited to exact matches or patterns that begin with
	 * '*' or end with '*'. Beyond this, a string cannot be validated as
	 * corresponding to an existing type or package (and this is probably not
	 * even desirable).
	 */
	private boolean validateEditorInput(final String trimmedValue) {
		final char firstChar = trimmedValue.charAt(0);
		if ((!(Character.isJavaIdentifierStart(firstChar)) || (firstChar == '*'))) {
			return false;
		}

		final int length = trimmedValue.length();
		for (int i = 1; i < length; i++) {
			final char c = trimmedValue.charAt(i);
			if (!Character.isJavaIdentifierPart(c)) {
				if (c == '.' && i != (length - 1)) {
					continue;
				}
				if (c == '*' && i == (length - 1)) {
					continue;
				}

				return false;
			}
		}
		return true;
	}

	private void addType() {
		// Shell shell = getShell();
		// SelectionDialog dialog = null;
		// try {
		// dialog = JavaUI.createTypeDialog(shell, PlatformUI.getWorkbench()
		// .getProgressService(), SearchEngine.createWorkspaceScope(),
		// IErlElementSearchConstants.CONSIDER_CLASSES, false);
		// } catch (JavaModelException jme) {
		// String title = JUnitMessages.JUnitPreferencePage_addtypedialog_title;
		// String message =
		// JUnitMessages.JUnitPreferencePage_addtypedialog_error_message;
		// ExceptionHandler.handle(jme, shell, title, message);
		// return;
		// }
		//
		// dialog.setTitle(JUnitMessages.JUnitPreferencePage_addtypedialog_title);
		// dialog
		// .setMessage(JUnitMessages.JUnitPreferencePage_addtypedialog_message);
		// if (dialog.open() == IDialogConstants.CANCEL_ID) {
		// return;
		// }
		//
		// Object[] types = dialog.getResult();
		// if (types != null && types.length > 0) {
		// IErlModule type = (IErlModule) types[0];
		// fStackFilterContentProvider.addFilter(type
		// .getFullyQualifiedName('.'), true);
		// }
	}

	private void addPackage() {
		// Shell shell = getShell();
		// IProgressService context = PlatformUI.getWorkbench()
		// .getProgressService();
		// IJavaSearchScope createWorkspaceScope = SearchEngine
		// .createWorkspaceScope();
		// SelectionDialog dialog = JavaUI.createPackageDialog(shell, context,
		// createWorkspaceScope, true, true, "");
		// dialog
		// .setTitle(JUnitMessages.JUnitPreferencePage_addpackagedialog_title);
		// dialog
		// .setMessage(JUnitMessages.JUnitPreferencePage_addpackagedialog_message);
		// if (dialog.open() != Window.OK) {
		// return;
		// }
		//
		// Object[] packages = dialog.getResult();
		// if (packages == null) {
		// return;
		// }
		//
		// for (int i = 0; i < packages.length; i++) {
		// IErlElement pkg = (IErlElement) packages[i];
		//
		// String filter = pkg.getElementName();
		// if (filter.length() < 1) {
		// filter = JUnitMessages.JUnitMainTab_label_defaultpackage;
		// } else {
		// filter += ".*";
		// }
		//
		// fStackFilterContentProvider.addFilter(filter, true);
		// }
	}

	private void removeFilters() {
		final IStructuredSelection selection = (IStructuredSelection) this.fFilterViewer
		.getSelection();
		this.fStackFilterContentProvider.removeFilters(selection.toArray());
	}

	private void checkAllFilters(final boolean check) {
		final Object[] filters = this.fStackFilterContentProvider.getElements(null);
		for (int i = (filters.length - 1); i >= 0; --i) {
			((Filter) filters[i]).setChecked(check);
		}

		this.fFilterViewer.setAllChecked(check);
	}

	@Override
	public boolean performOk() {
		this.fStackFilterContentProvider.saveFilters();
		return true;
	}

	@Override
	protected void performDefaults() {
		setDefaultValues();
		super.performDefaults();
	}

	private void setDefaultValues() {
		this.fStackFilterContentProvider.setDefaults();
	}

	/**
	 * Returns a list of active stack filters.
	 * 
	 * @return list
	 */
	protected List<String> createActiveStackFiltersList() {
		return Arrays.asList(getFilterPatterns());
	}

	/**
	 * Returns a list of active stack filters.
	 * 
	 * @return list
	 */
	protected List<String> createInactiveStackFiltersList() {
		final String[] strings = GUnitPreferencePage
		.parseList(getPreferenceStore().getString(
				GUnitPreferencesConstants.PREF_INACTIVE_FILTERS_LIST));
		return Arrays.asList(strings);
	}

	protected void updateActions() {
		if (this.fEnableAllButton == null) {
			return;
		}

		final boolean enabled = this.fFilterViewer.getTable().getItemCount() > 0;
		this.fEnableAllButton.setEnabled(enabled);
		this.fDisableAllButton.setEnabled(enabled);
	}

	public static String[] getFilterPatterns() {
		final IPreferenceStore store = GUnitPlugin.getDefault().getPreferenceStore();
		return GUnitPreferencePage.parseList(store
				.getString(GUnitPreferencesConstants.PREF_ACTIVE_FILTERS_LIST));
	}

	public static boolean getFilterStack() {
		final IPreferenceStore store = GUnitPlugin.getDefault().getPreferenceStore();
		return store.getBoolean(GUnitPreferencesConstants.DO_FILTER_STACK);
	}

	public static void setFilterStack(final boolean filter) {
		final IPreferenceStore store = GUnitPlugin.getDefault().getPreferenceStore();
		store.setValue(GUnitPreferencesConstants.DO_FILTER_STACK, filter);
	}

	/*
	 * Parses the comma separated string into an array of strings
	 */
	private static String[] parseList(final String listString) {
		final List<String> list = new ArrayList<String>(10);
		final StringTokenizer tokenizer = new StringTokenizer(listString, ","); //$NON-NLS-1$
		while (tokenizer.hasMoreTokens()) {
			list.add(tokenizer.nextToken());
		}
		return list.toArray(new String[list.size()]);
	}
}
