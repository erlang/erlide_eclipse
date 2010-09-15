package org.ttb.integration.views;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

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
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.IProgressService;
import org.eclipse.ui.statushandlers.StatusManager;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.util.ErlLogger;
import org.ttb.integration.Activator;
import org.ttb.integration.ProcessFlag;
import org.ttb.integration.ProcessMode;
import org.ttb.integration.TraceBackend;
import org.ttb.integration.TracingStatus;
import org.ttb.integration.mvc.controller.NodeCellModifier;
import org.ttb.integration.mvc.controller.NodeHelper;
import org.ttb.integration.mvc.controller.ProcessCellModifier;
import org.ttb.integration.mvc.controller.ProcessContentProvider;
import org.ttb.integration.mvc.controller.ProcessHelper;
import org.ttb.integration.mvc.controller.TracePatternCellModifier;
import org.ttb.integration.mvc.controller.TracePatternContentProvider;
import org.ttb.integration.mvc.model.ConfigurationManager;
import org.ttb.integration.mvc.model.ITraceNodeObserver;
import org.ttb.integration.mvc.model.TracePattern;
import org.ttb.integration.mvc.model.TracedNode;
import org.ttb.integration.mvc.model.TracedProcess;
import org.ttb.integration.mvc.view.MatchSpecCellEditor;
import org.ttb.integration.mvc.view.NodeColumn;
import org.ttb.integration.mvc.view.NodeContentProvider;
import org.ttb.integration.mvc.view.NodeLabelProvider;
import org.ttb.integration.mvc.view.ProcessColumn;
import org.ttb.integration.mvc.view.ProcessLabelProvider;
import org.ttb.integration.mvc.view.TracePatternColumn;
import org.ttb.integration.mvc.view.TracePatternLabelProvider;
import org.ttb.integration.ui.dialogs.BusyDialog;
import org.ttb.integration.ui.dialogs.ConfigurationSaveAsDialog;
import org.ttb.integration.ui.dialogs.SelectConfigurationDialog;

/**
 * Control panel for tracing settings.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class ControlPanelView extends ViewPart implements ITraceNodeObserver {

    private TableViewer functionsTableViewer;
    private TableViewer nodesTableViewer;
    private Composite currentProcessControl;
    private ProcessMode currentProcessMode = ProcessMode.ALL;
    private TableViewer processesTableViewer;
    private String patternsConfigName;
    private String nodesConfigName;
    private Action startStopAction;
    private BusyDialog busyDialog;

    public ControlPanelView() {
        TraceBackend.getInstance().addListener(this);
    }

    @Override
    public void dispose() {
        TraceBackend.getInstance().removeListener(this);
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
        TabFolder tabFolder = createTabs(parent);
        addNodesTab(tabFolder);
        addProcessesTab(tabFolder);
        addFunctionsTab(tabFolder);

        // initialize UI
        initializeUI();
    }

    private void initializeUI() {
        if (TraceBackend.getInstance().isStarted()) {
            doAfterStartTracing();
            if (TraceBackend.getInstance().isLoading()) {
                // tracing is being finished - data is being sent to eclipse
                doBeforeStopTracing();
            }
        } else {
            if (TraceBackend.getInstance().isLoading()) {
                // trace results from file are being loaded
                doBeforeLoading();
            } else
                // no action is being performed
                doAfterLoading();
        }
    }

    private void createActionBars() {
        // toolbar
        IToolBarManager manager = getViewSite().getActionBars().getToolBarManager();

        startStopAction = new Action() {
            @Override
            public void run() {
                if (!TraceBackend.getInstance().isLoading()) {
                    if (TraceBackend.getInstance().isStarted()) {
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

    private TabFolder createTabs(Composite parent) {
        TabFolder tabFolder = new TabFolder(parent, SWT.BORDER);
        tabFolder.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        return tabFolder;
    }

    /**
     * Method called when starting tracing.
     */
    private void doStartTracing() {
        IProgressService ps = PlatformUI.getWorkbench().getProgressService();
        try {
            ps.busyCursorWhile(new IRunnableWithProgress() {
                public void run(IProgressMonitor pm) {
                    TracingStatus status = TraceBackend.getInstance().start();
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
        TraceBackend.getInstance().stop();
    }

    /**
     * Method called after stopping tracing.
     */
    private void doAfterLoading() {
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
        TraceBackend.getInstance().setProcessMode(currentProcessMode);

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
                        TraceBackend.getInstance().setProcessMode(mode);
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

        TraceBackend.getInstance().removeAllProcessFlag();

        for (final ProcessFlag flag : ProcessFlag.values()) {
            final Button button = new Button(container, SWT.CHECK);
            button.setText(flag.getName());
            button.addSelectionListener(new SelectionAdapter() {

                @Override
                public void widgetSelected(SelectionEvent e) {
                    if (button.getSelection()) {
                        TraceBackend.getInstance().addProcessFlag(flag);
                    } else {
                        TraceBackend.getInstance().removeProcessFlag(flag);
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
        fillProcessesList(processesTableViewer);
        processesTableViewer.setInput(TraceBackend.getInstance().getProcesses());

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

    private void fillProcessesList(final TableViewer tableViewer) {
        IProgressService ps = PlatformUI.getWorkbench().getProgressService();
        try {
            ps.busyCursorWhile(new IRunnableWithProgress() {
                public void run(IProgressMonitor pm) {
                    TracedProcess[] processesList = ProcessHelper.getProcsOnTracedNodes();
                    TraceBackend.getInstance().setProcesses(processesList);
                }
            });
        } catch (Exception e) {
            ErlLogger.error(e);
        }
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
                TraceBackend.getInstance().addTracePattern(new TracePattern(true));
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
                    TraceBackend.getInstance().removeTracePattern(tracePattern);
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
                ElementListSelectionDialog dialog = new SelectConfigurationDialog(parent.getShell(), new LabelProvider());
                dialog.setElements(ConfigurationManager.getTPConfigs());
                dialog.open();
                String result = (String) dialog.getFirstResult();
                if (result != null) {
                    patternsConfigName = result;
                    configNameLabel.setText(patternsConfigName);
                    TraceBackend.getInstance().loadTracePatterns(ConfigurationManager.loadTPConfig(patternsConfigName));
                }
            }
        });

        // "Delete patterns" button
        deleteConfigButton.setToolTipText("Delete current pattern set...");
        deleteConfigButton.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_ELCL_REMOVE));
        deleteConfigButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                if (patternsConfigName != null) {
                    MessageBox messageBox = new MessageBox(parent.getShell(), SWT.ICON_QUESTION | SWT.YES | SWT.NO);
                    messageBox.setMessage("Delete \"" + patternsConfigName + "\"?");
                    messageBox.setText("Delete configuration");
                    if (messageBox.open() == SWT.YES) {
                        ConfigurationManager.removeTPConfig(patternsConfigName);
                        patternsConfigName = null;
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
                if (patternsConfigName != null) {
                    if (!ConfigurationManager.saveTPConfig(patternsConfigName)) {
                        MessageBox messageBox = new MessageBox(parent.getShell(), SWT.ICON_ERROR | SWT.OK);
                        messageBox.setMessage("Unable to save configuration: " + patternsConfigName);
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
                String[] configurations = ConfigurationManager.getTPConfigs();
                Set<String> existingNames = new HashSet<String>(Arrays.asList(configurations));
                InputDialog dialog = new ConfigurationSaveAsDialog(parent.getShell(), "Save trace pattern configuration", "Enter name for configuration:",
                        patternsConfigName, existingNames);

                if (dialog.open() == Window.OK) {
                    if (ConfigurationManager.saveTPConfig(dialog.getValue())) {
                        patternsConfigName = dialog.getValue();
                        configNameLabel.setText(patternsConfigName);
                    } else {
                        MessageBox messageBox = new MessageBox(parent.getShell(), SWT.ICON_ERROR | SWT.OK);
                        messageBox.setMessage("Unable to save configuration: " + dialog.getValue());
                        messageBox.setText("Error");
                        messageBox.open();
                    }
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

        // input
        functionsTableViewer.setInput(TraceBackend.getInstance());

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

    // "Nodes" tab methods

    private void addNodesTab(TabFolder tabFolder) {
        TabItem tabItem = new TabItem(tabFolder, SWT.NONE);
        tabItem.setText("Nodes");

        final Composite container = new Composite(tabFolder, SWT.NONE);
        final GridLayout containerLayout = new GridLayout(1, false);
        container.setLayout(containerLayout);
        containerLayout.marginWidth = 0;
        containerLayout.marginHeight = 0;
        containerLayout.verticalSpacing = 3;

        tabItem.setControl(container);
        createNodeButtonsPanel(container);
        createNodesTable(container);
    }

    private void createNodeButtonsPanel(final Composite parent) {
        Composite container = new Composite(parent, SWT.NONE);
        container.setLayout(new RowLayout());

        // "Add" button
        Button button = new Button(container, SWT.PUSH | SWT.CENTER);
        button.setText("New node");
        button.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_ADD));
        button.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                TraceBackend.getInstance().addTracedNode(new TracedNode());
            }
        });

        // "Remove" button
        button = new Button(container, SWT.PUSH | SWT.CENTER);
        button.setText("Remove node");
        button.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_TOOL_DELETE));
        button.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                TracedNode tracedNode = (TracedNode) ((IStructuredSelection) nodesTableViewer.getSelection()).getFirstElement();
                if (tracedNode != null) {
                    TraceBackend.getInstance().removeTracedNode(tracedNode);
                }
            }
        });

        // "Add erlide nodes" button
        button = new Button(container, SWT.PUSH | SWT.CENTER);
        button.setText("Add erlide nodes");
        button.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_ADD));
        button.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                for (Backend backend : NodeHelper.getBackends(true)) {
                    TracedNode node = new TracedNode();
                    node.setNodeName(backend.getPeer());
                    TraceBackend.getInstance().addTracedNode(node);
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

        // "Load nodes config" button
        loadConfigButton.setToolTipText("Load nodes configuration...");
        loadConfigButton.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
        loadConfigButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                ElementListSelectionDialog dialog = new SelectConfigurationDialog(parent.getShell(), new LabelProvider());
                dialog.setElements(ConfigurationManager.getNodesConfig());
                dialog.open();
                String result = (String) dialog.getFirstResult();
                if (result != null) {
                    nodesConfigName = result;
                    configNameLabel.setText(nodesConfigName);
                    TraceBackend.getInstance().loadTracedNodes(ConfigurationManager.loadNodesConfig(nodesConfigName));
                }
            }
        });

        // "Delete nodes configuration" button
        deleteConfigButton.setToolTipText("Delete current node configuration...");
        deleteConfigButton.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_ELCL_REMOVE));
        deleteConfigButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                if (nodesConfigName != null) {
                    MessageBox messageBox = new MessageBox(parent.getShell(), SWT.ICON_QUESTION | SWT.YES | SWT.NO);
                    messageBox.setMessage("Delete \"" + nodesConfigName + "\"?");
                    messageBox.setText("Delete configuration");
                    if (messageBox.open() == SWT.YES) {
                        ConfigurationManager.removeNodesConfig(nodesConfigName);
                        nodesConfigName = null;
                        configNameLabel.setText("");
                    }
                }
            }
        });

        // "Save nodes configuration" button
        saveConfigButton.setToolTipText("Save current nodes configuration");
        saveConfigButton.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_ETOOL_SAVE_EDIT));
        saveConfigButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                if (nodesConfigName != null) {
                    if (!ConfigurationManager.saveNodesConfig(nodesConfigName)) {
                        MessageBox messageBox = new MessageBox(parent.getShell(), SWT.ICON_ERROR | SWT.OK);
                        messageBox.setMessage("Unable to save configuration: " + nodesConfigName);
                        messageBox.setText("Error");
                        messageBox.open();
                    }
                }
            }
        });

        // "Save nodes configuration as..." button
        saveAsConfigButton.setToolTipText("Save current nodes configuration as...");
        saveAsConfigButton.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_ETOOL_SAVEAS_EDIT));
        saveAsConfigButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                String[] configurations = ConfigurationManager.getNodesConfig();
                Set<String> existingNames = new HashSet<String>(Arrays.asList(configurations));
                InputDialog dialog = new ConfigurationSaveAsDialog(parent.getShell(), "Save nodes configuration", "Enter name for configuration:",
                        nodesConfigName, existingNames);

                if (dialog.open() == Window.OK) {
                    if (ConfigurationManager.saveNodesConfig(dialog.getValue())) {
                        nodesConfigName = dialog.getValue();
                        configNameLabel.setText(nodesConfigName);
                    } else {
                        MessageBox messageBox = new MessageBox(parent.getShell(), SWT.ICON_ERROR | SWT.OK);
                        messageBox.setMessage("Unable to save configuration: " + dialog.getValue());
                        messageBox.setText("Error");
                        messageBox.open();
                    }
                }
            }
        });
    }

    private void createNodesTable(Composite parent) {
        int style = SWT.SINGLE | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.HIDE_SELECTION;
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.grabExcessVerticalSpace = true;

        nodesTableViewer = new TableViewer(parent, style);
        nodesTableViewer.setUseHashlookup(true);

        // table
        Table table = nodesTableViewer.getTable();
        table.setLayoutData(gridData);
        table.setHeaderVisible(true);
        table.setLinesVisible(true);

        // columns
        String columnProperties[] = new String[TracePatternColumn.values().length];
        for (NodeColumn column : NodeColumn.values()) {
            TableColumn tableColumn = new TableColumn(table, SWT.LEFT, column.ordinal());
            tableColumn.setResizable(true);
            tableColumn.setMoveable(false);
            tableColumn.setWidth(column.getWidth());
            tableColumn.setText(column.getName());
            columnProperties[column.ordinal()] = column.name();
        }
        nodesTableViewer.setColumnProperties(columnProperties);

        // providers
        nodesTableViewer.setLabelProvider(new NodeLabelProvider());
        nodesTableViewer.setContentProvider(new NodeContentProvider());

        // input
        nodesTableViewer.setInput(TraceBackend.getInstance());

        // editors
        CellEditor[] editors = new CellEditor[TracePatternColumn.values().length];
        editors[NodeColumn.ENABLED.ordinal()] = new CheckboxCellEditor(table);
        editors[NodeColumn.NODE_NAME.ordinal()] = new TextCellEditor(table);
        editors[NodeColumn.COOKIE.ordinal()] = new TextCellEditor(table);
        nodesTableViewer.setCellEditors(editors);
        nodesTableViewer.setCellModifier(new NodeCellModifier());
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
            Object errorObject = TraceBackend.getInstance().getErrorObject();
            status = new Status(IStatus.ERROR, Activator.PLUGIN_ID, message + ": " + errorObject, null);
            StatusManager.getManager().handle(status, StatusManager.SHOW);
            break;
        case EXCEPTION_THROWN:
            Exception e = (Exception) TraceBackend.getInstance().getErrorObject();
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

    public void addNode(TracedNode tracedNode) {
        nodesTableViewer.refresh();
    }

    public void removeNode(TracedNode tracedNode) {
        nodesTableViewer.refresh();
    }

    public void updateNode(TracedNode tracedNode) {
        nodesTableViewer.refresh();
    }

    public void loadNodes() {
        nodesTableViewer.refresh();
    }

    public void startTracing() {
        doAfterStartTracing();
    }

    public void receivedTraceData() {
    }

    public void startLoading() {
        doBeforeLoading();
    }

    public void finishLoading(TracingStatus status) {
        doAfterLoading();
        handleError(false, status);
    }
}
