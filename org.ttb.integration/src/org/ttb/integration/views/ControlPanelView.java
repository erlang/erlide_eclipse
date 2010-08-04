package org.ttb.integration.views;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.CheckboxCellEditor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.part.ViewPart;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.ErlideBackend;
import org.ttb.integration.TtbBackend;
import org.ttb.integration.mvc.controller.CellModifier;
import org.ttb.integration.mvc.controller.TracePatternContentProvider;
import org.ttb.integration.mvc.model.ITraceNodeObserver;
import org.ttb.integration.mvc.model.TracePattern;
import org.ttb.integration.mvc.view.Columns;
import org.ttb.integration.mvc.view.TracePatternLabelProvider;

/**
 * A control panel for tracing.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class ControlPanelView extends ViewPart implements ITraceNodeObserver {

    private TableViewer tableViewer;
    private Button startButton;
    private Combo backendNameCombo;

    public ControlPanelView() {
        TtbBackend.getInstance().addListener(this);
    }

    @Override
    public void dispose() {
        TtbBackend.getInstance().removeListener(this);
        super.dispose();
    }

    @Override
    public void createPartControl(Composite parent) {
        // layout
        final GridLayout containerLayout = new GridLayout(1, false);
        parent.setLayout(containerLayout);
        containerLayout.marginWidth = 0;
        containerLayout.marginHeight = 0;
        containerLayout.verticalSpacing = 3;

        // children
        createStartStopPanel(parent);
        TabFolder tabFolder = createTabs(parent);
        addProcessesTab(tabFolder);
        addFunctionsTab(tabFolder);
    }

    private TabFolder createTabs(Composite parent) {
        TabFolder tabFolder = new TabFolder(parent, SWT.BORDER);
        tabFolder.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        return tabFolder;
    }

    private void addProcessesTab(TabFolder tabFolder) {
        TabItem tabItem = new TabItem(tabFolder, SWT.NONE);
        tabItem.setText("Processes");

        final Composite container = new Composite(tabFolder, SWT.NONE);
        final GridLayout containerLayout = new GridLayout(2, false);
        container.setLayout(containerLayout);
        containerLayout.marginWidth = 0;
        containerLayout.marginHeight = 0;
        containerLayout.makeColumnsEqualWidth = false;
        containerLayout.verticalSpacing = 3;

        tabItem.setControl(container);
    }

    private void addFunctionsTab(TabFolder tabFolder) {
        TabItem tabItem = new TabItem(tabFolder, SWT.NONE);
        tabItem.setText("Functions");

        final Composite container = new Composite(tabFolder, SWT.NONE);
        final GridLayout containerLayout = new GridLayout(2, false);
        container.setLayout(containerLayout);
        containerLayout.marginWidth = 10;
        containerLayout.marginHeight = 0;
        containerLayout.makeColumnsEqualWidth = false;
        containerLayout.verticalSpacing = 3;

        tabItem.setControl(container);
        createPatternButtonsPanel(container);
        createTable(container);
    }

    private void createStartStopPanel(Composite parent) {
        final Composite container = new Composite(parent, SWT.NONE);
        final GridLayout containerLayout = new GridLayout(3, false);

        container.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        container.setLayout(containerLayout);
        containerLayout.marginWidth = 0;
        containerLayout.marginHeight = 0;
        containerLayout.makeColumnsEqualWidth = false;
        containerLayout.verticalSpacing = 3;

        backendNameCombo = new Combo(container, SWT.DROP_DOWN | SWT.READ_ONLY);
        backendNameCombo.setItems(getBackendNames());
        backendNameCombo.setLayoutData(new GridData(200, SWT.DEFAULT));

        Button refreshButton = new Button(container, SWT.PUSH | SWT.CENTER);
        refreshButton.setText("Refresh");
        refreshButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                backendNameCombo.setItems(getBackendNames());
            }
        });

        // "Start/Stop" button
        startButton = new Button(container, SWT.PUSH | SWT.CENTER);
        startButton.setText(TtbBackend.getInstance().isStarted() ? "Stop" : "Start");
        startButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                if (TtbBackend.getInstance().isStarted()) {
                    TtbBackend.getInstance().stop();
                } else {
                    TtbBackend.getInstance().start(ErlangCore.getBackendManager().getByName(backendNameCombo.getText()));
                }
            }
        });
    }

    private String[] getBackendNames() {
        Collection<ErlideBackend> backends = BackendManager.getDefault().getAllBackends();
        List<String> backendNames = new ArrayList<String>();
        for (ErlideBackend erlideBackend : backends) {
            backendNames.add(erlideBackend.getName());
        }
        Collections.sort(backendNames);
        return backendNames.toArray(new String[backendNames.size()]);
    }

    private void createPatternButtonsPanel(Composite parent) {
        // "Add" button
        Button button = new Button(parent, SWT.PUSH | SWT.CENTER);
        button.setText("Add");
        button.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                TtbBackend.getInstance().addTracePattern(new TracePattern());
            }
        });

        // "Remove" button
        button = new Button(parent, SWT.PUSH | SWT.CENTER);
        button.setText("Remove");
        button.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                TracePattern tracePattern = (TracePattern) ((IStructuredSelection) tableViewer.getSelection()).getFirstElement();
                if (tracePattern != null) {
                    TtbBackend.getInstance().removeTracePattern(tracePattern);
                }
            }
        });
    }

    private void createTable(Composite parent) {
        int style = SWT.SINGLE | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.HIDE_SELECTION;
        GridData gridData = new GridData(GridData.FILL_BOTH);
        gridData.grabExcessVerticalSpace = true;
        gridData.horizontalSpan = 3;

        tableViewer = new TableViewer(parent, style);
        tableViewer.setUseHashlookup(true);

        // table
        Table table = tableViewer.getTable();
        table.setLayoutData(gridData);
        table.setHeaderVisible(true);
        table.setLinesVisible(true);

        // columns
        String columnProperties[] = new String[Columns.values().length];
        for (Columns column : Columns.values()) {
            TableColumn tableColumn = new TableColumn(table, SWT.CENTER, column.ordinal());
            tableColumn.setResizable(true);
            tableColumn.setMoveable(true);
            tableColumn.setWidth(column.getWidth());
            tableColumn.setText(column.getName());
            columnProperties[column.ordinal()] = column.name();
        }
        tableViewer.setColumnProperties(columnProperties);

        // providers
        tableViewer.setLabelProvider(new TracePatternLabelProvider());
        tableViewer.setContentProvider(new TracePatternContentProvider());

        // editors
        CellEditor[] editors = new CellEditor[Columns.values().length];
        editors[Columns.ENABLED.ordinal()] = new CheckboxCellEditor(table);
        editors[Columns.LOCAL.ordinal()] = new CheckboxCellEditor(table);
        editors[Columns.MODULE_NAME.ordinal()] = new TextCellEditor(table);
        editors[Columns.FUNCTION_NAME.ordinal()] = new TextCellEditor(table);
        tableViewer.setCellEditors(editors);
        tableViewer.setCellModifier(new CellModifier());
    }

    @Override
    public void setFocus() {
        // TODO Auto-generated method stub
    }

    @Override
    public void addPattern(TracePattern tracePattern) {
        tableViewer.add(tracePattern);
    }

    @Override
    public void removePattern(TracePattern tracePattern) {
        tableViewer.remove(tracePattern);
    }

    @Override
    public void updatePattern(TracePattern tracePattern) {
        tableViewer.update(tracePattern, null);
    }

    @Override
    public void startTracing() {
        startButton.setText("Stop");
    }

    @Override
    public void stopTracing() {
        startButton.setText("Start");
    }
}
