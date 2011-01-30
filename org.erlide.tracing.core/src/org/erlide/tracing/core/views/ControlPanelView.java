package org.erlide.tracing.core.views;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
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
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.tracing.core.ITraceNodeObserver;
import org.erlide.tracing.core.ProcessFlag;
import org.erlide.tracing.core.ProcessMode;
import org.erlide.tracing.core.TraceBackend;
import org.erlide.tracing.core.TracingStatus;
import org.erlide.tracing.core.mvc.controller.NodeCellModifier;
import org.erlide.tracing.core.mvc.controller.NodeHelper;
import org.erlide.tracing.core.mvc.controller.ProcessCellModifier;
import org.erlide.tracing.core.mvc.controller.ProcessHelper;
import org.erlide.tracing.core.mvc.controller.TracePatternCellModifier;
import org.erlide.tracing.core.mvc.model.TracePattern;
import org.erlide.tracing.core.mvc.model.TracedNode;
import org.erlide.tracing.core.mvc.model.TracedProcess;
import org.erlide.tracing.core.mvc.view.MatchSpecCellEditor;
import org.erlide.tracing.core.mvc.view.NodeColumn;
import org.erlide.tracing.core.mvc.view.NodeContentProvider;
import org.erlide.tracing.core.mvc.view.NodeLabelProvider;
import org.erlide.tracing.core.mvc.view.ProcessColumn;
import org.erlide.tracing.core.mvc.view.ProcessContentProvider;
import org.erlide.tracing.core.mvc.view.ProcessLabelProvider;
import org.erlide.tracing.core.mvc.view.TracePatternColumn;
import org.erlide.tracing.core.mvc.view.TracePatternContentProvider;
import org.erlide.tracing.core.mvc.view.TracePatternLabelProvider;
import org.erlide.tracing.core.ui.dialogs.ConfigurationSaveAsDialog;
import org.erlide.tracing.core.ui.dialogs.RunnableWithProgress;
import org.erlide.tracing.core.ui.dialogs.SelectConfigurationDialog;
import org.erlide.tracing.core.utils.ConfigurationManager;
import org.erlide.tracing.core.utils.TracingStatusHandler;

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
    private RunnableWithProgress task;
    private TracingStatus status;

    private static final String START_LABEL = "Start tracing";
    private static final String STOP_LABEL = "Stop tracing";

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
    }

    private void createActionBars() {
        startStopAction = new Action() {
            @Override
            public void run() {
                if (!TraceBackend.getInstance().isLoading()) {
                    if (TraceBackend.getInstance().isStarted()) {
                        startStopAction.setEnabled(false);
                        doStopTracing();
                    } else {
                        doStartTracing();
                    }
                }
            }
        };
        startStopAction.setImageDescriptor(DebugUITools.getImageDescriptor(IDebugUIConstants.IMG_ACT_RUN));
        startStopAction.setToolTipText(START_LABEL);
        getViewSite().getActionBars().getToolBarManager().add(startStopAction);
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
        try {
            task = new RunnableWithProgress("Starting tracing") {

                @Override
                public void doAction() {
                    status = TraceBackend.getInstance().start();
                    finish();
                }
            };
            PlatformUI.getWorkbench().getProgressService().busyCursorWhile(task);
            TracingStatusHandler.handleStatus(status);
        } catch (Exception e) {
        } finally {
            task = null;
        }
    }

    /**
     * Method called when stopping tracing.
     */
    private void doStopTracing() {
        task = new RunnableWithProgress("Loading trace results...") {
            @Override
            public void doAction() {
                TraceBackend.getInstance().stop();
            }
        };
        try {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
            new ProgressMonitorDialog(shell).run(true, false, task);
            startStopAction.setImageDescriptor(DebugUITools.getImageDescriptor(IDebugUIConstants.IMG_ACT_RUN));
            startStopAction.setToolTipText(START_LABEL);
            startStopAction.setEnabled(true);
            TracingStatusHandler.handleStatus(status);
        } catch (Exception e) {
            ErlLogger.error(e);
        } finally {
            task = null;
        }
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
        button.setToolTipText("Add new trace pattern");
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
        button.setToolTipText("Remove selected trace pattern");
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
        functionsTableViewer.setCellModifier(new TracePatternCellModifier(functionsTableViewer));
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
        button.setToolTipText("Add new node you want to trace");
        button.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_ADD));
        button.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                TraceBackend.getInstance().addTracedNode(new TracedNode());
                nodesTableViewer.refresh();
            }
        });

        // "Remove" button
        button = new Button(container, SWT.PUSH | SWT.CENTER);
        button.setText("Remove node");
        button.setToolTipText("Remove selected node");
        button.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_TOOL_DELETE));
        button.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                TracedNode tracedNode = (TracedNode) ((IStructuredSelection) nodesTableViewer.getSelection()).getFirstElement();
                if (tracedNode != null) {
                    TraceBackend.getInstance().removeTracedNode(tracedNode);
                    nodesTableViewer.refresh();
                }
            }
        });

        // "Add erlide nodes" button
        button = new Button(container, SWT.PUSH | SWT.CENTER);
        button.setText("Add existing nodes");
        button.setToolTipText("Add all Erlang nodes started directly from eclipse");
        button.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_ADD));
        button.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                for (Backend backend : NodeHelper.getBackends(true)) {
                    TracedNode node = new TracedNode();
                    node.setNodeName(backend.getPeer());
                    TraceBackend.getInstance().addTracedNode(node);
                }
                nodesTableViewer.refresh();
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
                    nodesTableViewer.refresh();
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
        nodesTableViewer.setCellModifier(new NodeCellModifier(nodesTableViewer));
    }

    @Override
    public void setFocus() {
    }

    public void startTracing() {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                startStopAction.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_ELCL_STOP));
                startStopAction.setToolTipText(STOP_LABEL);
            }
        });
    }

    public void finishLoadingFile(TracingStatus theStatus) {
        this.status = theStatus;
        if (task != null) {
            task.finish();
        }
    }

    public void finishLoadingTraces(TracingStatus theStatus) {
    }

    public void removeFile() {
    }

    public void updateTracePatterns() {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                functionsTableViewer.refresh();
            }
        });
    }
}
