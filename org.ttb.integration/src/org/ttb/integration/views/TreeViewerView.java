package org.ttb.integration.views;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.IDebugUIConstants;
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
import org.erlide.core.erlang.util.ErlangFunction;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.ui.util.ErlModelUtils;
import org.ttb.integration.Activator;
import org.ttb.integration.TraceBackend;
import org.ttb.integration.TracingStatus;
import org.ttb.integration.mvc.controller.TreeContentProvider;
import org.ttb.integration.mvc.model.ITraceNodeObserver;
import org.ttb.integration.mvc.model.TraceCollections;
import org.ttb.integration.mvc.model.treenodes.FunctionNode;
import org.ttb.integration.mvc.model.treenodes.ITreeNode;
import org.ttb.integration.mvc.model.treenodes.ModuleNode;
import org.ttb.integration.mvc.model.treenodes.TracingResultsNode;
import org.ttb.integration.mvc.view.TreeLabelProvider;
import org.ttb.integration.preferences.PreferenceNames;
import org.ttb.integration.ui.dialogs.BusyDialog;

public class TreeViewerView extends ViewPart implements ITraceNodeObserver {

    private TreeViewer treeViewer;
    private Long index;
    private boolean correctInput = false;
    private Text traceIndexField;

    private BusyDialog busyDialog;
    private Composite buttonsPanel;
    private Button previousButton;
    private Button nextButton;
    private Button showButton;

    private Label label;

    public TreeViewerView() {
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
        containerLayout.marginWidth = 0;
        containerLayout.marginHeight = 0;
        containerLayout.verticalSpacing = 3;
        parent.setLayout(containerLayout);

        // children
        createTreeViewerPanel(parent);
        createButtonsPanel(parent);
    }

    private void createTreeViewerPanel(Composite parent) {
        final Composite container = new Composite(parent, SWT.NONE);
        container.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        container.setLayout(new GridLayout());

        // treeViewer = new TreeViewer(container, SWT.VIRTUAL);
        treeViewer = new TreeViewer(container);
        treeViewer.getTree().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        // providers
        treeViewer.setContentProvider(new TreeContentProvider(treeViewer, true));
        treeViewer.setLabelProvider(new TreeLabelProvider());

        // input
        treeViewer.setInput(TraceCollections.getTracesList());

        // listener
        treeViewer.addDoubleClickListener(new IDoubleClickListener() {

            public void doubleClick(DoubleClickEvent event) {
                doDoubleClick(event);
            }
        });
    }

    private void createButtonsPanel(Composite parent) {
        buttonsPanel = new Composite(parent, SWT.NONE);
        buttonsPanel.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));
        buttonsPanel.setLayout(new RowLayout());

        // "Previous" button
        previousButton = new Button(buttonsPanel, SWT.PUSH | SWT.CENTER);
        previousButton.setToolTipText("Show previous trace set");
        previousButton.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_TOOL_BACK));
        previousButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
                busyDialog = new BusyDialog(shell, "Loading trace results...");
                Display.getDefault().asyncExec(new Runnable() {
                    public void run() {
                        int limit = Activator.getDefault().getPreferenceStore().getInt(PreferenceNames.TRACES_LOAD_LIMIT);
                        long startIndex = Math.max(1L, index - limit);
                        long endIndex = startIndex + limit - 1;
                        TraceBackend.getInstance().loadDataFromFile(startIndex, endIndex);
                        busyDialog.start();
                    }
                });
            }
        });

        // "Next" button
        nextButton = new Button(buttonsPanel, SWT.PUSH | SWT.CENTER);
        nextButton.setToolTipText("Show next trace set");
        nextButton.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_TOOL_FORWARD));
        nextButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
                busyDialog = new BusyDialog(shell, "Loading trace results...");
                Display.getDefault().asyncExec(new Runnable() {
                    public void run() {
                        int limit = Activator.getDefault().getPreferenceStore().getInt(PreferenceNames.TRACES_LOAD_LIMIT);
                        long endIndex = Math.min(index + limit * 2 - 1, TraceBackend.getInstance().getActiveResultSet().getSize());
                        long startIndex = endIndex - limit + 1;
                        TraceBackend.getInstance().loadDataFromFile(startIndex, endIndex);
                        busyDialog.start();
                    }
                });
            }
        });

        // "Show" button
        showButton = new Button(buttonsPanel, SWT.PUSH | SWT.CENTER);
        showButton.setToolTipText("Show selected trace set");
        showButton.setImage(DebugUITools.getImage(IDebugUIConstants.IMG_OBJS_LAUNCH_RUN));
        showButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                doSelection();
            }
        });

        // Text field
        traceIndexField = new Text(buttonsPanel, SWT.SINGLE | SWT.BORDER);
        traceIndexField.setToolTipText("Select index of first trace event to display");
        traceIndexField.setLayoutData(new RowData(60, SWT.DEFAULT));
        traceIndexField.addListener(SWT.Modify, new Listener() {

            public void handleEvent(Event event) {
                try {
                    correctInput = false;
                    Long value = new Long(traceIndexField.getText());

                    if (value >= 1 && value <= TraceBackend.getInstance().getActiveResultSet().getSize()) {
                        index = value;
                        showButton.setEnabled(nextButton.isEnabled() || previousButton.isEnabled());
                        correctInput = true;
                    } else {
                        showButton.setEnabled(false);
                    }
                } catch (Exception e) {
                    showButton.setEnabled(false);
                }
            }
        });
        traceIndexField.addKeyListener(new KeyListener() {

            public void keyReleased(KeyEvent e) {
                if (e.keyCode == SWT.CR && correctInput) {
                    doSelection();
                }
            }

            public void keyPressed(KeyEvent e) {
            }
        });

        // label
        label = new Label(buttonsPanel, SWT.NONE);
        label.setLayoutData(new RowData(200, SWT.DEFAULT));

        updateButtonsPanel();
    }

    private void doSelection() {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
        busyDialog = new BusyDialog(shell, "Loading trace results...");
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                int limit = Activator.getDefault().getPreferenceStore().getInt(PreferenceNames.TRACES_LOAD_LIMIT);
                TraceBackend.getInstance().loadDataFromFile(index, index + limit - 1);
                busyDialog.start();
            }
        });
    }

    private void updateButtonsPanel() {
        TracingResultsNode resultSet = TraceBackend.getInstance().getActiveResultSet();
        if (resultSet != null) {
            index = TraceBackend.getInstance().getStartIndex();
            int size = TraceCollections.getTracesList().size();
            boolean previousEnabled = index > 1;
            boolean nextEnabled = index + size - 1 < resultSet.getSize();

            previousButton.setEnabled(previousEnabled);
            nextButton.setEnabled(nextEnabled);
            showButton.setEnabled(previousEnabled || nextEnabled);
            traceIndexField.setEnabled(previousEnabled || nextEnabled);
            traceIndexField.setText(String.valueOf(index));
            buttonsPanel.setEnabled(true);

            StringBuilder stringBuilder = new StringBuilder(" (");
            if (resultSet.getSize() == 0)
                stringBuilder.append("no traces)");
            else
                stringBuilder.append(index).append(" - ").append(index + size - 1).append(" of ").append(resultSet.getSize()).append(" traces)");
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
    private void doDoubleClick(DoubleClickEvent event) {
        IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        ITreeNode treeNode = (ITreeNode) selection.getFirstElement();
        try {
            if (treeNode instanceof FunctionNode) {
                FunctionNode functionNode = (FunctionNode) treeNode;
                ErlangFunction erlangFunction = new ErlangFunction(functionNode.getFunctionName(), functionNode.getArity());
                ErlModelUtils.openExternalFunction(functionNode.getModuleName(), erlangFunction, null, null, null, true);
            } else if (treeNode instanceof ModuleNode) {
                ModuleNode moduleNode = (ModuleNode) treeNode;
                ErlModelUtils.openExternalType(moduleNode.getModuleName(), moduleNode.getModuleName(), null, null, null, true);
            }
        } catch (CoreException e) {
            ErlLogger.error(e);
        }
    }

    @Override
    public void setFocus() {
    }

    public void startTracing() {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                buttonsPanel.setEnabled(false);
            }
        });
    }

    public void finishLoadingFile(final TracingStatus status) {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                if (TracingStatus.OK.equals(status))
                    treeViewer.refresh();
                if (busyDialog != null)
                    busyDialog.finish();
            }
        });
    }

    public void finishLoadingTraces(final TracingStatus status) {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                if (TracingStatus.OK.equals(status)) {
                    updateButtonsPanel();
                    treeViewer.refresh();
                }
                if (busyDialog != null)
                    busyDialog.finish();
            }
        });
    }

    public void clearTraceLists() {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                updateButtonsPanel();
                treeViewer.refresh();
            }
        });
    }

    public void updateTracePatterns() {
    }
}
