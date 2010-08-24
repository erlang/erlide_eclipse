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
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.part.ViewPart;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.backend.Backend;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.ErlideBackend;
import org.ttb.integration.ProcessFlag;
import org.ttb.integration.ProcessMode;
import org.ttb.integration.TtbBackend;
import org.ttb.integration.mvc.controller.ProcessCellModifier;
import org.ttb.integration.mvc.controller.ProcessContentProvider;
import org.ttb.integration.mvc.controller.TracePatternCellModifier;
import org.ttb.integration.mvc.controller.TracePatternContentProvider;
import org.ttb.integration.mvc.model.ITraceNodeObserver;
import org.ttb.integration.mvc.model.ProcessOnList;
import org.ttb.integration.mvc.model.TracePattern;
import org.ttb.integration.mvc.view.MatchSpecCellEditor;
import org.ttb.integration.mvc.view.ProcessColumn;
import org.ttb.integration.mvc.view.ProcessLabelProvider;
import org.ttb.integration.mvc.view.TracePatternColumn;
import org.ttb.integration.mvc.view.TracePatternLabelProvider;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideProclist;

/**
 * Control panel for tracing settings.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class ControlPanelView extends ViewPart implements ITraceNodeObserver {

    private TableViewer functionsTableViewer;
    private Button startButton;
    private Combo backendNameCombo;
    private Composite currentProcessControl;
    private ProcessMode currentProcessMode = ProcessMode.ALL;
    private TableViewer processesTableViewer;

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

    private void createStartStopPanel(Composite parent) {
        final Composite container = new Composite(parent, SWT.NONE);
        final GridLayout containerLayout = new GridLayout(3, false);
        container.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        container.setLayout(containerLayout);
        containerLayout.marginWidth = 0;
        containerLayout.marginHeight = 0;
        containerLayout.makeColumnsEqualWidth = false;
        containerLayout.verticalSpacing = 3;

        // backend combo box
        backendNameCombo = new Combo(container, SWT.DROP_DOWN | SWT.READ_ONLY);
        backendNameCombo.setItems(getBackendNames());
        backendNameCombo.setLayoutData(new GridData(250, SWT.DEFAULT));

        // "Refresh" button
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
        // TODO what if tracing is stopped, i.e. viewer hasn't received yet all
        // data? In this case button should be disabled
        startButton.setText(TtbBackend.getInstance().isStarted() ? "Stop" : "Start");
        startButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                if (TtbBackend.getInstance().isStarted()) {
                    startButton.setEnabled(false);
                    TtbBackend.getInstance().stop();
                } else {
                    ArrayList<Backend> backends = new ArrayList<Backend>();
                    backends.add(ErlangCore.getBackendManager().getByName(backendNameCombo.getText()));
                    TtbBackend.getInstance().start(backends);
                }
            }
        });
    }

    private TabFolder createTabs(Composite parent) {
        TabFolder tabFolder = new TabFolder(parent, SWT.BORDER);
        tabFolder.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        return tabFolder;
    }

    // "Processes" tab methods

    private void addProcessesTab(TabFolder tabFolder) {
        TabItem tabItem = new TabItem(tabFolder, SWT.NONE);
        tabItem.setText("Processes");

        final Composite container = new Composite(tabFolder, SWT.NONE);
        final GridLayout containerLayout = new GridLayout(1, false);
        container.setLayout(containerLayout);
        containerLayout.marginWidth = 0;
        containerLayout.marginHeight = 0;
        containerLayout.makeColumnsEqualWidth = false;
        containerLayout.verticalSpacing = 3;

        tabItem.setControl(container);
        createProcessRadioButtons(container);
        createProcessControl(currentProcessMode, null, container);
    }

    private void createProcessRadioButtons(final Composite parent) {
        TtbBackend.getInstance().setProcessMode(currentProcessMode);

        final Composite container = new Composite(parent, SWT.NONE);
        container.setLayout(new GridLayout(4, false));
        container.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        for (final ProcessMode mode : ProcessMode.values()) {
            final Button button = new Button(container, SWT.RADIO);
            button.setText(mode.getName());
            button.setSelection(currentProcessMode.equals(mode));
            button.addSelectionListener(new SelectionAdapter() {

                @Override
                public void widgetSelected(SelectionEvent e) {
                    if (button.getSelection()) {
                        TtbBackend.getInstance().setProcessMode(mode);
                        createProcessControl(mode, currentProcessMode, parent);
                        currentProcessMode = mode;
                    }
                }
            });
        }
    }

    /**
     * Creates either checkboxes for setting global flags or table for setting
     * flags individually for each process.
     * 
     * @param newMode
     * @param oldMode
     * @param parent
     */
    private void createProcessControl(ProcessMode newMode, ProcessMode oldMode, Composite parent) {
        boolean changeToByPidMode = ProcessMode.BY_PID.equals(newMode) && (oldMode == null || !ProcessMode.BY_PID.equals(oldMode));
        boolean changeFromByPidMode = !ProcessMode.BY_PID.equals(newMode) && (oldMode == null || ProcessMode.BY_PID.equals(oldMode));

        if (currentProcessControl != null && (changeToByPidMode || changeFromByPidMode))
            currentProcessControl.dispose();

        if (changeFromByPidMode) {
            currentProcessControl = createProcessCheckBoxes(parent);
            parent.layout(true);
        } else if (changeToByPidMode) {
            currentProcessControl = createProcessesTable(parent);
            parent.layout(true);
        }
    }

    private Composite createProcessCheckBoxes(Composite parent) {
        final Composite container = new Composite(parent, SWT.NONE);
        container.setLayout(new RowLayout());

        TtbBackend.getInstance().removeAllProcessFlag();

        for (final ProcessFlag flag : ProcessFlag.values()) {
            final Button button = new Button(container, SWT.CHECK);
            button.setText(flag.getName());
            button.addSelectionListener(new SelectionAdapter() {

                @Override
                public void widgetSelected(SelectionEvent e) {
                    if (button.getSelection()) {
                        TtbBackend.getInstance().addProcessFlag(flag);
                    } else {
                        TtbBackend.getInstance().removeProcessFlag(flag);
                    }
                }
            });
        }
        return container;
    }

    private Composite createProcessesTable(Composite parent) {
        final Composite container = new Composite(parent, SWT.NONE);
        container.setLayout(new GridLayout());
        container.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        int style = SWT.SINGLE | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.HIDE_SELECTION;
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.grabExcessVerticalSpace = true;

        processesTableViewer = new TableViewer(container, style);
        processesTableViewer.setUseHashlookup(true);

        // table
        Table table = processesTableViewer.getTable();
        table.setLayoutData(gridData);
        table.setHeaderVisible(true);
        table.setLinesVisible(true);

        // columns
        String columnProperties[] = new String[ProcessColumn.values().length + ProcessFlag.values().length];

        // columns: process properties
        for (ProcessColumn column : ProcessColumn.values()) {
            TableColumn tableColumn = new TableColumn(table, SWT.LEFT, column.ordinal());
            tableColumn.setResizable(true);
            tableColumn.setMoveable(false);
            tableColumn.setWidth(column.getWidth());
            tableColumn.setText(column.getName());
            columnProperties[column.ordinal()] = column.name();
        }
        // columns: process flags
        for (ProcessFlag flag : ProcessFlag.values()) {
            TableColumn tableColumn = new TableColumn(table, SWT.CENTER, ProcessColumn.values().length + flag.ordinal());
            tableColumn.setResizable(true);
            tableColumn.setMoveable(false);
            tableColumn.setWidth(60);
            tableColumn.setText(flag.getName());
            columnProperties[ProcessColumn.values().length + flag.ordinal()] = flag.name();
        }
        processesTableViewer.setColumnProperties(columnProperties);

        // providers
        processesTableViewer.setLabelProvider(new ProcessLabelProvider());
        processesTableViewer.setContentProvider(new ProcessContentProvider());

        // input
        ProcessOnList[] processesList = getProcessesList();
        if (processesList != null) {
            processesTableViewer.setInput(processesList);
            TtbBackend.getInstance().setProcesses(processesList);
        }

        // editors
        CellEditor[] editors = new CellEditor[ProcessFlag.values().length + ProcessFlag.values().length];
        editors[ProcessColumn.SELECTED.ordinal()] = new CheckboxCellEditor(table);
        for (ProcessFlag flag : ProcessFlag.values()) {
            editors[ProcessColumn.values().length + flag.ordinal()] = new CheckboxCellEditor(table);
        }
        processesTableViewer.setCellEditors(editors);
        processesTableViewer.setCellModifier(new ProcessCellModifier(processesTableViewer));

        return container;
    }

    private ProcessOnList[] getProcessesList() {
        if (ErlangCore.getBackendManager().getByName(backendNameCombo.getText()) != null) {
            OtpErlangList processList = ErlideProclist.getProcessList(ErlangCore.getBackendManager().getByName(backendNameCombo.getText()));

            ProcessOnList[] processes = new ProcessOnList[processList.arity()];
            for (int i = 0; i < processList.arity(); i++) {
                processes[i] = new ProcessOnList((OtpErlangTuple) processList.elementAt(i));
            }
            return processes;
        }
        return null;
    }

    // "Functions" tab methods

    private void addFunctionsTab(TabFolder tabFolder) {
        TabItem tabItem = new TabItem(tabFolder, SWT.NONE);
        tabItem.setText("Functions");

        final Composite container = new Composite(tabFolder, SWT.NONE);
        final GridLayout containerLayout = new GridLayout(1, false);
        container.setLayout(containerLayout);
        containerLayout.marginWidth = 0;
        containerLayout.marginHeight = 0;
        containerLayout.verticalSpacing = 3;

        tabItem.setControl(container);
        createPatternButtonsPanel(container);
        createFunctionsTable(container);
    }

    private void createPatternButtonsPanel(Composite parent) {
        Composite container = new Composite(parent, SWT.NONE);
        container.setLayout(new RowLayout());

        // "Add" button
        Button button = new Button(container, SWT.PUSH | SWT.CENTER);
        button.setText("Add");
        button.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                TtbBackend.getInstance().addTracePattern(new TracePattern());
            }
        });

        // "Remove" button
        button = new Button(container, SWT.PUSH | SWT.CENTER);
        button.setText("Remove");
        button.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                TracePattern tracePattern = (TracePattern) ((IStructuredSelection) functionsTableViewer.getSelection()).getFirstElement();
                if (tracePattern != null) {
                    TtbBackend.getInstance().removeTracePattern(tracePattern);
                }
            }
        });
    }

    private void createFunctionsTable(Composite parent) {
        int style = SWT.SINGLE | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.HIDE_SELECTION;
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.grabExcessVerticalSpace = true;

        functionsTableViewer = new TableViewer(parent, style);
        functionsTableViewer.setUseHashlookup(true);

        // table
        Table table = functionsTableViewer.getTable();
        table.setLayoutData(gridData);
        table.setHeaderVisible(true);
        table.setLinesVisible(true);

        // columns
        String columnProperties[] = new String[TracePatternColumn.values().length];
        for (TracePatternColumn column : TracePatternColumn.values()) {
            TableColumn tableColumn = new TableColumn(table, SWT.LEFT, column.ordinal());
            tableColumn.setResizable(true);
            tableColumn.setMoveable(false);
            tableColumn.setWidth(column.getWidth());
            tableColumn.setText(column.getName());
            columnProperties[column.ordinal()] = column.name();
        }
        functionsTableViewer.setColumnProperties(columnProperties);

        // providers
        functionsTableViewer.setLabelProvider(new TracePatternLabelProvider());
        functionsTableViewer.setContentProvider(new TracePatternContentProvider());

        // editors
        CellEditor[] editors = new CellEditor[TracePatternColumn.values().length];
        editors[TracePatternColumn.ENABLED.ordinal()] = new CheckboxCellEditor(table);
        editors[TracePatternColumn.LOCAL.ordinal()] = new CheckboxCellEditor(table);
        editors[TracePatternColumn.MODULE_NAME.ordinal()] = new TextCellEditor(table);
        editors[TracePatternColumn.FUNCTION_NAME.ordinal()] = new TextCellEditor(table);
        editors[TracePatternColumn.ARITY.ordinal()] = new TextCellEditor(table);
        editors[TracePatternColumn.MATCH_SPEC.ordinal()] = new MatchSpecCellEditor(table);
        functionsTableViewer.setCellEditors(editors);
        functionsTableViewer.setCellModifier(new TracePatternCellModifier());
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

    @Override
    public void setFocus() {
    }

    public void addPattern(TracePattern tracePattern) {
        functionsTableViewer.add(tracePattern);
    }

    public void removePattern(TracePattern tracePattern) {
        functionsTableViewer.remove(tracePattern);
    }

    public void updatePattern(TracePattern tracePattern) {
        functionsTableViewer.update(tracePattern, null);
    }

    public void startTracing() {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                startButton.setText("Stop");
            }
        });
    }

    public void stopTracing() {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                startButton.setText("Start");
                startButton.setEnabled(true);
            }
        });
    }

    public void receivedTraceData() {
    }

    public void startLoading() {
        startButton.setEnabled(false);
    }

    public void stopLoading() {
        startButton.setEnabled(true);
    }
}
