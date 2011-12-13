package org.erlide.tracing.core.views;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;
import org.erlide.jinterface.ErlLogger;
import org.erlide.tracing.core.Activator;
import org.erlide.tracing.core.ITraceNodeObserver;
import org.erlide.tracing.core.TraceBackend;
import org.erlide.tracing.core.TracingStatus;
import org.erlide.tracing.core.mvc.model.TraceCollections;
import org.erlide.tracing.core.mvc.model.treenodes.FunctionNode;
import org.erlide.tracing.core.mvc.model.treenodes.ITreeNode;
import org.erlide.tracing.core.mvc.model.treenodes.ModuleNode;
import org.erlide.tracing.core.mvc.model.treenodes.TracingResultsNode;
import org.erlide.tracing.core.mvc.view.TreeContentProvider;
import org.erlide.tracing.core.mvc.view.TreeLabelProvider;
import org.erlide.tracing.core.preferences.PreferenceNames;
import org.erlide.tracing.core.ui.dialogs.RunnableWithProgress;
import org.erlide.tracing.core.utils.TracingStatusHandler;
import org.erlide.ui.util.ErlModelUtils;

public class TreeViewerView extends ViewPart implements ITraceNodeObserver {

    private TreeViewer treeViewer;
    private Long index;
    private boolean correctInput = false;
    private Text traceIndexField;

    private RunnableWithProgress task;
    private Composite buttonsPanel;
    private Button previousButton;
    private Button nextButton;
    private Button showButton;

    private Label label;
    private TracingStatus status;

    public TreeViewerView() {
        TraceBackend.getInstance().addListener(this);
    }

    @Override
    public void dispose() {
        TraceBackend.getInstance().removeListener(this);
        super.dispose();
    }

    @Override
    public void createPartControl(final Composite parent) {
        // layout
        final GridLayout containerLayout = new GridLayout(1, false);
        containerLayout.marginWidth = 0;
        containerLayout.marginHeight = 0;
        containerLayout.verticalSpacing = 3;
        parent.setLayout(containerLayout);

        // children
        createTreeViewerPanel(parent);
        createButtonsPanel(parent);
    }

    private void createTreeViewerPanel(final Composite parent) {
        final Composite container = new Composite(parent, SWT.NONE);
        container.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        container.setLayout(new GridLayout());

        // treeViewer = new TreeViewer(container, SWT.VIRTUAL);
        treeViewer = new TreeViewer(container, SWT.SINGLE);
        treeViewer.getTree().setLayoutData(
                new GridData(SWT.FILL, SWT.FILL, true, true));

        // providers
        treeViewer
                .setContentProvider(new TreeContentProvider(treeViewer, true));
        treeViewer.setLabelProvider(new TreeLabelProvider());

        // input
        treeViewer.setInput(TraceCollections.getTracesList());

        // listener
        treeViewer.addDoubleClickListener(new IDoubleClickListener() {

            @Override
            public void doubleClick(final DoubleClickEvent event) {
                doDoubleClick(event);
            }
        });
    }

    private void createButtonsPanel(final Composite parent) {
        buttonsPanel = new Composite(parent, SWT.NONE);
        buttonsPanel
                .setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));
        buttonsPanel.setLayout(new RowLayout());

        // "Previous" button
        previousButton = new Button(buttonsPanel, SWT.PUSH | SWT.CENTER);
        previousButton.setToolTipText("Show previous trace set");
        previousButton.setImage(PlatformUI.getWorkbench().getSharedImages()
                .getImage(ISharedImages.IMG_TOOL_BACK));
        previousButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(final SelectionEvent e) {
                task = new RunnableWithProgress("Loading trace results...") {
                    @Override
                    public void doAction() {
                        final int limit = Activator.getDefault()
                                .getPreferenceStore()
                                .getInt(PreferenceNames.TRACES_LOAD_LIMIT);
                        final long startIndex = Math.max(1L, index - limit);
                        final long endIndex = startIndex + limit - 1;
                        TraceBackend.getInstance().loadDataFromFile(startIndex,
                                endIndex);
                    }
                };
                executeTask();
            }
        });

        // "Next" button
        nextButton = new Button(buttonsPanel, SWT.PUSH | SWT.CENTER);
        nextButton.setToolTipText("Show next trace set");
        nextButton.setImage(PlatformUI.getWorkbench().getSharedImages()
                .getImage(ISharedImages.IMG_TOOL_FORWARD));
        nextButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(final SelectionEvent e) {
                task = new RunnableWithProgress("Loading trace results...") {
                    @Override
                    public void doAction() {
                        final int limit = Activator.getDefault()
                                .getPreferenceStore()
                                .getInt(PreferenceNames.TRACES_LOAD_LIMIT);
                        final long endIndex = Math.min(index + limit * 2 - 1,
                                TraceBackend.getInstance().getActiveResultSet()
                                        .getSize());
                        final long startIndex = endIndex - limit + 1;
                        TraceBackend.getInstance().loadDataFromFile(startIndex,
                                endIndex);
                    }
                };
                executeTask();
            }
        });

        // "Show" button
        showButton = new Button(buttonsPanel, SWT.PUSH | SWT.CENTER);
        showButton.setToolTipText("Show selected trace set");
        showButton.setImage(DebugUITools
                .getImage(IDebugUIConstants.IMG_OBJS_LAUNCH_RUN));
        showButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(final SelectionEvent e) {
                doSelection();
            }
        });

        // Text field
        traceIndexField = new Text(buttonsPanel, SWT.SINGLE | SWT.BORDER);
        traceIndexField
                .setToolTipText("Select index of first trace event to display");
        traceIndexField.setLayoutData(new RowData(60, SWT.DEFAULT));
        traceIndexField.addListener(SWT.Modify, new Listener() {

            @Override
            public void handleEvent(final Event event) {
                try {
                    correctInput = false;
                    final Long value = new Long(traceIndexField.getText());

                    if (value >= 1
                            && value <= TraceBackend.getInstance()
                                    .getActiveResultSet().getSize()) {
                        index = value;
                        showButton.setEnabled(nextButton.isEnabled()
                                || previousButton.isEnabled());
                        correctInput = true;
                    } else {
                        showButton.setEnabled(false);
                    }
                } catch (final Exception e) {
                    showButton.setEnabled(false);
                }
            }
        });
        traceIndexField.addKeyListener(new KeyListener() {

            @Override
            public void keyReleased(final KeyEvent e) {
                if (e.keyCode == SWT.CR && correctInput) {
                    doSelection();
                }
            }

            @Override
            public void keyPressed(final KeyEvent e) {
            }
        });

        // label
        label = new Label(buttonsPanel, SWT.NONE);
        label.setLayoutData(new RowData(200, SWT.DEFAULT));

        updateButtonsPanel();
    }

    private void doSelection() {
        task = new RunnableWithProgress("Loading trace results...") {
            @Override
            public void doAction() {
                final int limit = Activator.getDefault().getPreferenceStore()
                        .getInt(PreferenceNames.TRACES_LOAD_LIMIT);
                TraceBackend.getInstance().loadDataFromFile(index,
                        index + limit - 1);
            }
        };
        executeTask();
    }

    private void executeTask() {
        try {
            final Shell shell = PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getShell();
            new ProgressMonitorDialog(shell).run(true, false, task);
            doAfterLoadingTraces();
        } catch (final Exception exception) {
            ErlLogger.error(exception);
        } finally {
            task = null;
        }
    }

    private void updateButtonsPanel() {
        final TracingResultsNode resultSet = TraceBackend.getInstance()
                .getActiveResultSet();
        if (resultSet != null) {
            index = TraceBackend.getInstance().getStartIndex();
            final int size = TraceCollections.getTracesList().size();
            final boolean previousEnabled = index > 1;
            final boolean nextEnabled = index + size - 1 < resultSet.getSize();

            previousButton.setEnabled(previousEnabled);
            nextButton.setEnabled(nextEnabled);
            showButton.setEnabled(previousEnabled || nextEnabled);
            traceIndexField.setEnabled(previousEnabled || nextEnabled);
            traceIndexField.setText(String.valueOf(index));
            buttonsPanel.setEnabled(true);

            final StringBuilder stringBuilder = new StringBuilder(" (");
            if (resultSet.getSize() == 0) {
                stringBuilder.append("no traces)");
            } else {
                stringBuilder.append(index).append(" - ")
                        .append(index + size - 1).append(" of ")
                        .append(resultSet.getSize()).append(" traces)");
            }
            label.setText(stringBuilder.toString());
        } else {
            traceIndexField.setText("");
            label.setText("");
            buttonsPanel.setEnabled(false);
        }
    }

    /**
     * Action performed when user double-clicks on tree element.
     * 
     * @param event
     */
    private void doDoubleClick(final DoubleClickEvent event) {
        final IStructuredSelection selection = (IStructuredSelection) event
                .getSelection();
        final ITreeNode treeNode = (ITreeNode) selection.getFirstElement();
        try {
            if (treeNode instanceof FunctionNode) {
                final FunctionNode functionNode = (FunctionNode) treeNode;
                ErlModelUtils
                        .openMFA(functionNode.getModuleName(),
                                functionNode.getFunctionName(),
                                functionNode.getArity());
            } else if (treeNode instanceof ModuleNode) {
                final ModuleNode moduleNode = (ModuleNode) treeNode;
                ErlModelUtils.openModule(moduleNode.getModuleName());
            }
        } catch (final CoreException e) {
            ErlLogger.error(e);
        }
    }

    private void doAfterLoadingTraces() {
        if (TracingStatus.OK.equals(status)) {
            updateButtonsPanel();
            treeViewer.refresh();
        }
        if (task != null) {
            // task was executed from this class so this class is responsible
            // for handling status
            TracingStatusHandler.handleStatus(status);
        }
    }

    @Override
    public void setFocus() {
    }

    @Override
    public void startTracing() {
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                buttonsPanel.setEnabled(false);
            }
        });
    }

    @Override
    public void finishLoadingFile(final TracingStatus theStatus) {
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                if (TracingStatus.OK.equals(theStatus)) {
                    treeViewer.refresh();
                }
            }
        });
    }

    @Override
    public void finishLoadingTraces(final TracingStatus theStatus) {
        status = theStatus;
        if (task != null) {
            // when loading was initialized from this view
            task.finish();
        } else {
            // when loading was initialized outside this view
            Display.getDefault().asyncExec(new Runnable() {
                @Override
                public void run() {
                    doAfterLoadingTraces();
                }
            });
        }
    }

    @Override
    public void removeFile() {
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                updateButtonsPanel();
                treeViewer.refresh();
            }
        });
    }

    @Override
    public void updateTracePatterns() {
    }
}
