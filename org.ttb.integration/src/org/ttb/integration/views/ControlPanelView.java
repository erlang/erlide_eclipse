package org.ttb.integration.views;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.CheckboxCellEditor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.IProgressService;
import org.eclipse.ui.statushandlers.StatusManager;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.backend.Backend;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.ErlideBackend;
import org.ttb.integration.Activator;
import org.ttb.integration.ProcessFlag;
import org.ttb.integration.ProcessMode;
import org.ttb.integration.TracingStatus;
import org.ttb.integration.TtbBackend;
import org.ttb.integration.mvc.controller.ProcessCellModifier;
import org.ttb.integration.mvc.controller.ProcessContentProvider;
import org.ttb.integration.mvc.controller.TracePatternCellModifier;
import org.ttb.integration.mvc.controller.TracePatternContentProvider;
import org.ttb.integration.mvc.model.ConfigurationManager;
import org.ttb.integration.mvc.model.ITraceNodeObserver;
import org.ttb.integration.mvc.model.ProcessOnList;
import org.ttb.integration.mvc.model.TracePattern;
import org.ttb.integration.mvc.view.MatchSpecCellEditor;
import org.ttb.integration.mvc.view.ProcessColumn;
import org.ttb.integration.mvc.view.ProcessLabelProvider;
import org.ttb.integration.mvc.view.TracePatternColumn;
import org.ttb.integration.mvc.view.TracePatternLabelProvider;
import org.ttb.integration.ui.dialogs.BusyDialog;
import org.ttb.integration.ui.dialogs.SelectTracingConfigurationDialog;
import org.ttb.integration.ui.dialogs.TracingConfigurationSaveAsDialog;

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
    private Combo backendNameCombo;
    private Composite currentProcessControl;
    private ProcessMode currentProcessMode = ProcessMode.ALL;
    private TableViewer processesTableViewer;
    private String configName;
    private Action startStopAction;
    private BusyDialog busyDialog;

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

        // toolbars and menu
        createActionBars();

        // children
        createStartStopPanel(parent);
        TabFolder tabFolder = createTabs(parent);
        addProcessesTab(tabFolder);
        addFunctionsTab(tabFolder);

        // initialize UI
        initializeUI();
    }

    private void initializeUI() {
        if (TtbBackend.getInstance().isStarted()) {
            doAfterStartTracing();
            if (TtbBackend.getInstance().isLoading()) {
                // tracing is being finished - data is being sent to eclipse
                doBeforeStopTracing();
            }
        } else {
            if (TtbBackend.getInstance().isLoading()) {
                // trace results from file are being loaded
                doBeforeLoading();
            } else
                // no action is being performed
                doAfterStopTracing();
        }
    }

    private void createActionBars() {
        // toolbar
        IToolBarManager manager = getViewSite().getActionBars().getToolBarManager();

        startStopAction = new Action() {
            @Override
            public void run() {
                if (!TtbBackend.getInstance().isLoading()) {
                    if (TtbBackend.getInstance().isStarted()) {
                        doBeforeStopTracing();
                        doStopTracing();
                    } else {
                        doStartTracing();
                    }
                }
            }
        };

        startStopAction.setImageDescriptor(DebugUITools.getImageDescriptor(IDebugUIConstants.IMG_ACT_RUN));
        manager.add(startStopAction);
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
    }

    private TabFolder createTabs(Composite parent) {
        TabFolder tabFolder = new TabFolder(parent, SWT.BORDER);
        tabFolder.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        return tabFolder;
    }

    /**
     * Method called when starting tracing.
     */
    private void doStartTracing() {
        final String nodeName = backendNameCombo.getText();
        IWorkbench wb = PlatformUI.getWorkbench();
        IProgressService ps = wb.getProgressService();
        try {
            ps.busyCursorWhile(new IRunnableWithProgress() {
                public void run(IProgressMonitor pm) {
                    ArrayList<Backend> backends = new ArrayList<Backend>();
                    Backend backendName = ErlangCore.getBackendManager().getByName(nodeName);
                    if (backendName != null) {
                        backends.add(backendName);
                    }
                    TracingStatus status = TtbBackend.getInstance().start(backends);
                    handleError(true, status);
                }
            });
        } catch (Exception e) {
        }
    }

    /**
     * Method called after starting tracing.
     */
    private void doAfterStartTracing() {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                startStopAction.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_ELCL_STOP));
                startStopAction.setToolTipText("Stop tracing");
            }
        });
    }

    /**
     * Method called before stopping tracing.
     */
    private void doBeforeStopTracing() {
        startStopAction.setEnabled(false);
    }

    /**
     * Method called when stopping tracing.
     */
    private void doStopTracing() {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
        busyDialog = new BusyDialog(shell, "Loading trace results...");
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                busyDialog.start();
            }
        });
        TtbBackend.getInstance().stop();
    }

    /**
     * Method called after stopping tracing.
     */
    private void doAfterStopTracing() {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                startStopAction.setImageDescriptor(DebugUITools.getImageDescriptor(IDebugUIConstants.IMG_ACT_RUN));
                startStopAction.setToolTipText("Start tracing");
                startStopAction.setEnabled(true);
                if (busyDialog != null)
                    busyDialog.finish();
            }
        });
    }

    /**
     * Method called before loading trace results.
     */
    private void doBeforeLoading() {
        startStopAction.setEnabled(false);
    }

    /**
     * Method called after loading trace results.
     */
    private void doAfterLoading() {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                startStopAction.setEnabled(true);
            }
        });
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

    private void createPatternButtonsPanel(final Composite parent) {
        Composite container = new Composite(parent, SWT.NONE);
        container.setLayout(new RowLayout());

        // "Add" button
        Button button = new Button(container, SWT.PUSH | SWT.CENTER);
        button.setText("New pattern");
        button.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_ADD));
        button.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                TtbBackend.getInstance().addTracePattern(new TracePattern(true));
            }
        });

        // "Remove" button
        button = new Button(container, SWT.PUSH | SWT.CENTER);
        button.setText("Remove pattern");
        button.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_TOOL_DELETE));
        button.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                TracePattern tracePattern = (TracePattern) ((IStructuredSelection) functionsTableViewer.getSelection()).getFirstElement();
                if (tracePattern != null) {
                    TtbBackend.getInstance().removeTracePattern(tracePattern);
                }
            }
        });

        // Pattern config buttons
        Button loadConfigButton = new Button(container, SWT.PUSH | SWT.CENTER);
        Button deleteConfigButton = new Button(container, SWT.PUSH | SWT.CENTER);
        Button saveConfigButton = new Button(container, SWT.PUSH | SWT.CENTER);
        Button saveAsConfigButton = new Button(container, SWT.PUSH | SWT.CENTER);
        final Label configNameLabel = new Label(container, SWT.NULL);
        configNameLabel.setLayoutData(new RowData(120, SWT.DEFAULT));

        // "Load patterns" button
        loadConfigButton.setToolTipText("Load pattern set...");
        loadConfigButton.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
        loadConfigButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                ElementListSelectionDialog dialog = new SelectTracingConfigurationDialog(parent.getShell(), new LabelProvider());
                dialog.open();
                String result = (String) dialog.getFirstResult();
                if (result != null) {
                    configName = result;
                    configNameLabel.setText(configName);
                    TtbBackend.getInstance().loadTracePatterns(ConfigurationManager.loadTracePatterns(configName));
                }
            }
        });

        // "Delete patterns" button
        deleteConfigButton.setToolTipText("Delete current pattern set");
        deleteConfigButton.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_ELCL_REMOVE));
        deleteConfigButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                if (configName != null) {
                    MessageBox messageBox = new MessageBox(parent.getShell(), SWT.ICON_QUESTION | SWT.YES | SWT.NO);
                    messageBox.setMessage("Delete \"" + configName + "\"?");
                    messageBox.setText("Delete configuration");
                    if (messageBox.open() == SWT.YES) {
                        ConfigurationManager.removeTracingPatterns(configName);
                        configName = null;
                        configNameLabel.setText("");
                    }
                }
            }
        });

        // "Save patterns" button
        saveConfigButton.setToolTipText("Save current pattern set");
        saveConfigButton.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_ETOOL_SAVE_EDIT));
        saveConfigButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                if (configName != null) {
                    if (!ConfigurationManager.saveTracePatterns(configName)) {
                        MessageBox messageBox = new MessageBox(parent.getShell(), SWT.ICON_ERROR | SWT.OK);
                        messageBox.setMessage("Unable to save configuration: " + configName);
                        messageBox.setText("Error");
                        messageBox.open();
                    }
                }
            }
        });

        // "Save patterns as..." button
        saveAsConfigButton.setToolTipText("Save current pattern set as...");
        saveAsConfigButton.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_ETOOL_SAVEAS_EDIT));
        saveAsConfigButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                InputDialog dialog = new TracingConfigurationSaveAsDialog(parent.getShell(), "Save trace pattern configuration",
                        "Enter name for configuration:", configName);
                if (dialog.open() == Window.OK) {
                    if (ConfigurationManager.saveTracePatterns(dialog.getValue())) {
                        configName = dialog.getValue();
                        configNameLabel.setText(configName);
                    } else {
                        MessageBox messageBox = new MessageBox(parent.getShell(), SWT.ICON_ERROR | SWT.OK);
                        messageBox.setMessage("Unable to save configuration: " + dialog.getValue());
                        messageBox.setText("Error");
                        messageBox.open();
                    }
                }
            }
        });

        // configNameLabel.setParent(container);
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

        // input
        functionsTableViewer.setInput(TtbBackend.getInstance());

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

    /**
     * Handles errors that occurred during starting tracing or loading data.
     * 
     * @param start
     *            <code>true</code> if handling errors during starting,
     *            <code>false</code> otherwise
     * @param tracingStatus
     *            status
     */
    private void handleError(boolean start, TracingStatus tracingStatus) {
        Status status;
        String message = start ? "Could not start tracing" : "Error while loading data";
        switch (tracingStatus) {
        case ERROR:
            Object errorObject = TtbBackend.getInstance().getErrorObject();
            status = new Status(IStatus.ERROR, Activator.PLUGIN_ID, message + ": " + errorObject, null);
            StatusManager.getManager().handle(status, StatusManager.SHOW);
            break;
        case EXCEPTION_THROWN:
            Exception e = (Exception) TtbBackend.getInstance().getErrorObject();
            status = new Status(IStatus.ERROR, Activator.PLUGIN_ID, message, e);
            StatusManager.getManager().handle(status, StatusManager.SHOW);
            break;
        case NO_ACTIVATED_NODES:
            status = new Status(IStatus.ERROR, Activator.PLUGIN_ID, "No nodes were activated for tracing", null);
            StatusManager.getManager().handle(status, StatusManager.SHOW);
            break;
        case OK:
            break;
        }
    }

    public void addPattern(TracePattern tracePattern) {
        functionsTableViewer.refresh();
    }

    public void removePattern(TracePattern tracePattern) {
        functionsTableViewer.refresh();
    }

    public void updatePattern(TracePattern tracePattern) {
        functionsTableViewer.refresh();
    }

    public void loadPatterns() {
        functionsTableViewer.refresh();
    }

    public void startTracing() {
        doAfterStartTracing();
    }

    public void stopTracing(TracingStatus status) {
        doAfterStopTracing();
        handleError(false, status);
    }

    public void receivedTraceData() {
    }

    public void startLoading() {
        doBeforeLoading();
    }

    public void stopLoading(TracingStatus status) {
        // doAfterLoading();
        doAfterStopTracing();
        handleError(false, status);
    }
}
