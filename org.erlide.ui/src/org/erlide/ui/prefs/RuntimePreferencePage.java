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
import org.eclipse.jface.dialogs.IMessageProvider;
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
import org.erlide.backend.BackendCore;
import org.erlide.backend.runtimeinfo.RuntimeInfo;
import org.erlide.backend.runtimeinfo.RuntimeInfoManager;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.util.SWTUtil;

/**
 * A preference page that displays installed runtimes in a table. Runtimes can
 * be added, removed, edited, and searched for.
 * <p>
 * It implements ISelectionProvider - it sends selection change events when the
 * checked runtime in the table changes, or when the "use default" button check
 * state changes.
 * </p>
 */
public class RuntimePreferencePage extends PreferencePage implements
        IAddDialogRequestor<RuntimeInfo>, ISelectionProvider,
        IWorkbenchPreferencePage {

    private final RuntimeInfoManager manager;
    private Combo combo;
    private static final String RUNTIMES_PREFERENCE_PAGE = "RUNTIMES_PREFERENCE_PAGE";

    Collection<RuntimeInfo> runtimes;
    RuntimeInfo defaultRuntime;
    RuntimeInfo erlideRuntime;

    /**
     * The main list control
     */
    protected CheckboxTableViewer fRuntimeList;

    // Action buttons
    private Button fAddButton;
    private Button fRemoveButton;
    private Button fEditButton;
    private Button fDuplicateButton;

    // column weights
    protected float fWeight1 = 1 / 4F;
    protected float fWeight2 = 4 / 9F;

    // ignore column re-sizing when the table is being resized
    protected boolean fResizingTable = false;

    // index of column used for sorting
    private int fSortColumn = 0;

    /**
     * Selection listeners (checked Backend changes)
     */
    private final ListenerList fSelectionListeners = new ListenerList();

    /**
     * Previous selection
     */
    private ISelection fPrevSelection = new StructuredSelection();

    public RuntimePreferencePage() {
        super();
        setTitle("Installed Erlang runtimes ");
        setDescription("Add, remove or edit runtime definitions.\n"
                + "The checked one will be used by default in new projects "
                + "to build the project's code.");
        manager = BackendCore.getRuntimeInfoManager();
        performDefaults();
    }

    @Override
    public void addSelectionChangedListener(
            final ISelectionChangedListener listener) {
        fSelectionListeners.add(listener);
    }

    @Override
    public ISelection getSelection() {
        return new StructuredSelection(fRuntimeList.getCheckedElements());
    }

    @Override
    public void removeSelectionChangedListener(
            final ISelectionChangedListener listener) {
        fSelectionListeners.remove(listener);
    }

    @Override
    public void setSelection(final ISelection selection) {
        if (selection instanceof IStructuredSelection) {
            if (!selection.equals(fPrevSelection)) {
                fPrevSelection = selection;
                final Object vm = ((IStructuredSelection) selection)
                        .getFirstElement();
                if (vm == null) {
                    fRuntimeList.setCheckedElements(new Object[0]);
                } else {
                    fRuntimeList.setCheckedElements(new Object[] { vm });
                    fRuntimeList.reveal(vm);
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
        fRuntimeList.setSorter(new ViewerSorter() {

            @Override
            public int compare(final Viewer viewer, final Object e1,
                    final Object e2) {
                if (e1 instanceof RuntimeInfo && e2 instanceof RuntimeInfo) {
                    final RuntimeInfo left = (RuntimeInfo) e1;
                    final RuntimeInfo right = (RuntimeInfo) e2;
                    return left.getName().compareToIgnoreCase(right.getName());
                }
                return super.compare(viewer, e1, e2);
            }

            @Override
            public boolean isSorterProperty(final Object element,
                    final String property) {
                return true;
            }
        });
        fSortColumn = 1;
    }

    /**
     * Sorts by VM location.
     */
    protected void sortByDirectory() {
        fRuntimeList.setSorter(new ViewerSorter() {

            @Override
            public int compare(final Viewer viewer, final Object e1,
                    final Object e2) {
                if (e1 instanceof RuntimeInfo && e2 instanceof RuntimeInfo) {
                    final RuntimeInfo left = (RuntimeInfo) e1;
                    final RuntimeInfo right = (RuntimeInfo) e2;
                    return left.getOtpHome().compareToIgnoreCase(
                            right.getOtpHome());
                }
                return super.compare(viewer, e1, e2);
            }

            @Override
            public boolean isSorterProperty(final Object element,
                    final String property) {
                return true;
            }
        });
        fSortColumn = 2;
    }

    protected void enableButtons() {
        final int selectionCount = ((IStructuredSelection) fRuntimeList
                .getSelection()).size();
        fEditButton.setEnabled(selectionCount == 1);
        fDuplicateButton.setEnabled(selectionCount == 1);
        fRemoveButton.setEnabled(selectionCount > 0);
    }

    protected Button createPushButton(final Composite parent, final String label) {
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
            public void controlResized(final ControlEvent e) {
                resizeTable(parent, buttons, table, column1, column2, column3);
            }
        });
        table.addListener(SWT.Paint, new Listener() {

            @Override
            public void handleEvent(final Event event) {
                table.removeListener(SWT.Paint, this);
                resizeTable(parent, buttons, table, column1, column2, column3);
            }
        });
        column1.addControlListener(new ControlAdapter() {

            @Override
            public void controlResized(final ControlEvent e) {
                if (column1.getWidth() > 0 && !fResizingTable) {
                    fWeight1 = getColumnWeight(0);
                }
            }
        });
        column2.addControlListener(new ControlAdapter() {

            @Override
            public void controlResized(final ControlEvent e) {
                if (column2.getWidth() > 0 && !fResizingTable) {
                    fWeight2 = getColumnWeight(1);
                }
            }
        });
    }

    protected void resizeTable(final Composite parent, final Composite buttons,
            final Table table, final TableColumn column1,
            final TableColumn column2, final TableColumn column3) {
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
    public List<RuntimeInfo> getRuntimes() {
        return new ArrayList<RuntimeInfo>(runtimes);
    }

    /**
     * Bring up a dialog that lets the user create a new runtime definition.
     */
    protected void addRuntime() {
        fRuntimeList.refresh();

        final AddRuntimeDialog dialog = new AddRuntimeDialog(this, getShell(),
                null, true);
        dialog.setTitle(RuntimePreferenceMessages.add_title);
        if (dialog.open() != Window.OK) {
            return;
        }
        fRuntimeList.refresh();
    }

    @Override
    public void itemAdded(final RuntimeInfo vm) {
        runtimes.add(vm);
        fRuntimeList.refresh();
        selectSingle();
    }

    private void selectSingle() {
        if (runtimes.size() == 1) {
            final RuntimeInfo vm = runtimes.iterator().next();
            fRuntimeList.setChecked(vm, true);
        }
        if (runtimes.size() > 1) {
            final RuntimeInfo vm = runtimes.iterator().next();
            if (fRuntimeList.getCheckedElements().length == 0) {
                fRuntimeList.setChecked(vm, true);
            }
        }
    }

    /**
     * @see IAddRuntimeDialogRequestor#hasRuntimeWithName(String)
     */
    @Override
    public boolean isDuplicateName(final String name) {
        return manager.hasRuntimeWithName(name);
    }

    protected void editRuntime() {
        final IStructuredSelection selection = (IStructuredSelection) fRuntimeList
                .getSelection();
        final RuntimeInfo vm = (RuntimeInfo) selection.getFirstElement();
        if (vm == null) {
            return;
        }
        final AddRuntimeDialog dialog = new AddRuntimeDialog(this, getShell(),
                vm, false);
        dialog.setTitle(RuntimePreferenceMessages.edit_title);
        if (dialog.open() != Window.OK) {
            return;
        }
        fRuntimeList.refresh(vm);
    }

    protected void duplicateRuntime() {
        final IStructuredSelection selection = (IStructuredSelection) fRuntimeList
                .getSelection();
        final RuntimeInfo vm = (RuntimeInfo) selection.getFirstElement();
        if (vm == null) {
            return;
        }
        final RuntimeInfo vm1 = RuntimeInfo.copy(vm, true);
        final AddRuntimeDialog dialog = new AddRuntimeDialog(this, getShell(),
                vm1, true);
        dialog.setTitle(RuntimePreferenceMessages.edit_title);
        if (dialog.open() != Window.OK) {
            return;
        }
        fRuntimeList.refresh();
    }

    protected void removeSelectedRuntimes() {
        final IStructuredSelection selection = (IStructuredSelection) fRuntimeList
                .getSelection();
        final RuntimeInfo[] vms = new RuntimeInfo[selection.size()];
        final Iterator<?> iter = selection.iterator();
        int i = 0;
        while (iter.hasNext()) {
            vms[i] = (RuntimeInfo) iter.next();
            i++;
        }
        removeRuntimes(vms);
    }

    /**
     * Removes the given VMs from the table.
     * 
     * @param vms
     */
    public void removeRuntimes(final RuntimeInfo[] vms) {
        final IStructuredSelection prev = (IStructuredSelection) getSelection();
        if (runtimes.size() == 1) {
            setMessage("You can't delete the last runtime definition",
                    IMessageProvider.INFORMATION);
            return;
        }
        for (final RuntimeInfo element : vms) {
            runtimes.remove(element);
        }
        fRuntimeList.refresh();
        final IStructuredSelection curr = (IStructuredSelection) getSelection();
        if (!curr.equals(prev)) {
            final List<RuntimeInfo> installs = getRuntimes();
            if (curr.size() == 0 && installs.size() == 1) {
                // pick a default VM automatically
                setSelection(new StructuredSelection(installs.get(0)));
            } else {
                fireSelectionChanged();
            }
        }
        selectSingle();
    }

    @Override
    public Shell getShell() {
        return getControl().getShell();
    }

    /**
     * Sets the checked Backend, possible <code>null</code>
     * 
     * @param vm
     *            Backend or <code>null</code>
     */
    public void setCheckedRuntime(final RuntimeInfo vm) {
        if (vm == null) {
            setSelection(new StructuredSelection());
        } else {
            setSelection(new StructuredSelection(vm));
        }
    }

    /**
     * Returns the checked Backend or <code>null</code> if none.
     * 
     * @return the checked Backend or <code>null</code> if none
     */
    public RuntimeInfo getCheckedRuntime() {
        final Object[] objects = fRuntimeList.getCheckedElements();
        if (objects.length == 0) {
            return null;
        }
        return (RuntimeInfo) objects[0];
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
    public void saveColumnSettings(final IDialogSettings settings,
            final String qualifier) {
        for (int i = 0; i < 2; i++) {
            // persist the first 2 column weights
            settings.put(qualifier + ".column" + i, getColumnWeight(i)); //$NON-NLS-1$
        }
        settings.put(qualifier + ".sortColumn", fSortColumn); //$NON-NLS-1$
    }

    protected float getColumnWeight(final int col) {
        final Table table = fRuntimeList.getTable();
        final int tableWidth = table.getSize().x;
        final int columnWidth = table.getColumn(col).getWidth();
        if (tableWidth > columnWidth) {
            return (float) columnWidth / tableWidth;
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
    public void restoreColumnSettings(final IDialogSettings settings,
            final String qualifier) {
        fWeight1 = restoreColumnWeight(settings, qualifier, 0);
        fWeight2 = restoreColumnWeight(settings, qualifier, 1);
        fRuntimeList.getTable().layout(true);
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
            sortByDirectory();
            break;
        }
    }

    private float restoreColumnWeight(final IDialogSettings settings,
            final String qualifier, final int col) {
        try {
            return settings.getFloat(qualifier + ".column" + col); //$NON-NLS-1$
        } catch (final NumberFormatException e) {
            return 1 / 3F;
        }

    }

    @Override
    protected Control createContents(final Composite parent) {
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

            @Override
            public void selectionChanged(final SelectionChangedEvent event) {
                checkValid();
            }
        });

        checkValid();

        applyDialogFont(parent);
        return parent;
    }

    @Override
    public void init(final IWorkbench workbench) {
    }

    private Control createMyControl(final Composite ancestor) {
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

        final Label erlideLabel = new Label(composite, SWT.NONE);
        erlideLabel
                .setToolTipText("The erlide runtime is used for IDE purposes, not for running project code. "
                        + "It is the most recent stable version that is installed.");
        final String erlideName = erlideRuntime == null ? "none"
                : erlideRuntime.getName();
        erlideLabel
                .setText(RuntimePreferenceMessages.RuntimePreferencePage_erlideLabel_text
                        + erlideName);
        new Label(composite, SWT.NONE);
        new Label(parent, SWT.NONE);

        final Label tableLabel = new Label(parent, SWT.NONE);
        tableLabel.setText("Installed runtimes:");
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
                    final RuntimeInfo ri = (RuntimeInfo) e.item.getData();
                    defaultRuntime = ri;
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
        column1.setText(RuntimePreferenceMessages.name);
        column1.setResizable(true);
        column1.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(final SelectionEvent e) {
                sortByName();
            }
        });

        final TableColumn column2 = new TableColumn(table, SWT.NULL);
        column2.setWidth(150);
        column2.setText("Directory");
        column2.setResizable(true);
        column2.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(final SelectionEvent e) {
                sortByDirectory();
            }
        });

        final TableColumn column3 = new TableColumn(table, SWT.NULL);
        column3.setWidth(80);
        column3.setText("Version");
        column3.setResizable(false);

        fRuntimeList = new CheckboxTableViewer(table);
        fRuntimeList.setLabelProvider(new RuntimeLabelProvider());
        fRuntimeList.setContentProvider(new RuntimeContentProvider());
        fRuntimeList.setInput(runtimes);
        if (defaultRuntime != null) {
            fRuntimeList.setCheckedElements(new Object[] { defaultRuntime });
        }
        // by default, sort by name
        sortByName();

        fRuntimeList
                .addSelectionChangedListener(new ISelectionChangedListener() {

                    @Override
                    public void selectionChanged(final SelectionChangedEvent evt) {
                        enableButtons();
                    }
                });

        fRuntimeList.addCheckStateListener(new ICheckStateListener() {

            @Override
            public void checkStateChanged(final CheckStateChangedEvent event) {
                if (event.getChecked()) {
                    setCheckedRuntime((RuntimeInfo) event.getElement());
                } else {
                    setCheckedRuntime(null);
                }
            }
        });

        fRuntimeList.addDoubleClickListener(new IDoubleClickListener() {

            @Override
            public void doubleClick(final DoubleClickEvent e) {
                if (!fRuntimeList.getSelection().isEmpty()) {
                    editRuntime();
                }
            }
        });
        table.addKeyListener(new KeyAdapter() {

            @Override
            public void keyPressed(final KeyEvent event) {
                if (event.character == SWT.DEL && event.stateMask == 0) {
                    removeSelectedRuntimes();
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

        fAddButton = createPushButton(buttons, RuntimePreferenceMessages.add);
        fAddButton.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(final Event evt) {
                addRuntime();
            }
        });

        fEditButton = createPushButton(buttons, RuntimePreferenceMessages.edit);
        fEditButton.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(final Event evt) {
                editRuntime();
            }
        });

        fDuplicateButton = createPushButton(buttons,
                RuntimePreferenceMessages.duplicate);
        fDuplicateButton.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(final Event evt) {
                duplicateRuntime();
            }

        });

        fRemoveButton = createPushButton(buttons,
                RuntimePreferenceMessages.remove);
        fRemoveButton.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(final Event evt) {
                removeSelectedRuntimes();
            }
        });

        configureTableResizing(parent, buttons, table, column1, column2,
                column3);

        enableButtons();

        return parent;
    }

    @Override
    public boolean performOk() {
        if (defaultRuntime == null) {
            defaultRuntime = (RuntimeInfo) fRuntimeList.getElementAt(0);
        }
        manager.setDefaultRuntime(defaultRuntime.getName());
        manager.setRuntimes(runtimes);
        manager.store();

        // save column widths
        final IDialogSettings settings = ErlideUIPlugin.getDefault()
                .getDialogSettings();
        saveColumnSettings(settings, RUNTIMES_PREFERENCE_PAGE);

        return super.performOk();
    }

    @Override
    public void performDefaults() {
        runtimes = manager.getRuntimes();
        defaultRuntime = manager.getDefaultRuntime();
        erlideRuntime = manager.getErlideRuntime();
    }

    void checkValid() {
        final RuntimeInfo def = getCheckedRuntime();

        if (def == null && getRuntimes().size() > 0) {
            setValid(false);
            setErrorMessage("Please select a default runtime.");
        } else {
            setValid(true);
            setErrorMessage(null);
        }
    }

    /**
     * Content provider to show a list of Runtimes
     */
    class RuntimeContentProvider implements IStructuredContentProvider {

        @Override
        public Object[] getElements(final Object input) {
            return runtimes.toArray(new RuntimeInfo[runtimes.size()]);
        }

        @Override
        public void inputChanged(final Viewer viewer, final Object oldInput,
                final Object newInput) {
        }

        @Override
        public void dispose() {
        }

    }

    /**
     * Label provider for installed runtimes table.
     */
    static class RuntimeLabelProvider extends LabelProvider implements
            ITableLabelProvider {

        /**
         * @see ITableLabelProvider#getColumnText(Object, int)
         */
        @Override
        public String getColumnText(final Object element, final int columnIndex) {
            if (element instanceof RuntimeInfo) {
                final RuntimeInfo vm = (RuntimeInfo) element;
                switch (columnIndex) {
                case 0:
                    return vm.getName();
                case 1:
                    return vm.getOtpHome();
                case 2:
                    return vm.getVersion().toString();
                }
            }
            return element.toString();
        }

        /**
         * @see ITableLabelProvider#getColumnImage(Object, int)
         */
        @Override
        public Image getColumnImage(final Object element, final int columnIndex) {
            return null;
        }

        @Override
        public String getText(final Object element) {
            return getColumnText(element, 0);
        }
    }

}
