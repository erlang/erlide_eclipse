package org.ttb.integration.views;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.statushandlers.StatusManager;
import org.ttb.integration.Activator;
import org.ttb.integration.TraceBackend;
import org.ttb.integration.TracingStatus;
import org.ttb.integration.mvc.controller.TreeContentProvider;
import org.ttb.integration.mvc.model.ITraceNodeObserver;
import org.ttb.integration.mvc.model.TraceCollections;
import org.ttb.integration.mvc.model.treenodes.ITreeNode;
import org.ttb.integration.mvc.model.treenodes.TracingResultsNode;
import org.ttb.integration.mvc.view.TreeLabelProvider;
import org.ttb.integration.preferences.PreferenceNames;
import org.ttb.integration.ui.dialogs.BusyDialog;

/**
 * Sequence diagram which shows tracing results.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TraceBrowserView extends ViewPart implements ITraceNodeObserver {

    private TreeViewer treeViewer;
    private Action clearAction;
    private Action loadAction;
    private BusyDialog busyDialog;

    public TraceBrowserView() {
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

        // toolbars and menu
        createActionBars();

        // children
        createTreeViewerPanel(parent);
    }

    private void createActionBars() {
        IToolBarManager manager = getViewSite().getActionBars().getToolBarManager();

        loadAction = new Action() {
            @Override
            public void run() {
                DirectoryDialog dialog = new DirectoryDialog(PlatformUI.getWorkbench().getDisplay().getActiveShell(), SWT.OPEN);
                dialog.setFilterPath(ResourcesPlugin.getWorkspace().getRoot().getLocation().toString());
                // dialog.setFilterExtensions(new String[] { "*.*" });
                dialog.setText("Load trace data...");
                final String selected = dialog.open();
                if (selected != null) {
                    TraceBackend.getInstance().loadFile(selected);
                    Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
                    busyDialog = new BusyDialog(shell, "Loading trace results...");
                    busyDialog.start();
                }
            }
        };
        loadAction.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_OBJ_FOLDER));
        loadAction.setToolTipText("Load results from file...");

        clearAction = new Action() {
            @Override
            public void run() {
                TraceBackend.getInstance().clearTraceLists();
            }
        };
        clearAction.setImageDescriptor(DebugUITools.getImageDescriptor(IDebugUIConstants.IMG_LCL_REMOVE_ALL));
        clearAction.setToolTipText("Clear view");

        manager.add(loadAction);
        manager.add(clearAction);

        // enable or disable buttons depending on whether tracing is started or
        // not
        enableButtons(!TraceBackend.getInstance().isStarted());
    }

    private void enableButtons(boolean enabled) {
        loadAction.setEnabled(enabled);
        clearAction.setEnabled(enabled);
    }

    private void createTreeViewerPanel(Composite parent) {
        final Composite container = new Composite(parent, SWT.NONE);
        container.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        container.setLayout(new GridLayout());

        // treeViewer = new TreeViewer(container, SWT.VIRTUAL);
        treeViewer = new TreeViewer(container);
        treeViewer.getTree().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        // providers
        treeViewer.setContentProvider(new TreeContentProvider(treeViewer, false));
        treeViewer.setLabelProvider(new TreeLabelProvider());

        // input
        treeViewer.setInput(TraceCollections.getFilesList());

        // listener
        treeViewer.addSelectionChangedListener(new ISelectionChangedListener() {

            public void selectionChanged(SelectionChangedEvent event) {
                doSelection(event);
            }
        });
    }

    /**
     * Action performed when user clicks on tree element.
     * 
     * @param event
     */
    private void doSelection(final SelectionChangedEvent event) {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
        busyDialog = new BusyDialog(shell, "Loading trace results...");
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                IStructuredSelection selection = (IStructuredSelection) event.getSelection();
                ITreeNode treeNode = (ITreeNode) selection.getFirstElement();
                if (treeNode != null) {
                    TraceBackend.getInstance().setActiveResultSet((TracingResultsNode) treeNode);
                    int limit = Activator.getDefault().getPreferenceStore().getInt(PreferenceNames.TRACES_LOAD_LIMIT);
                    TraceBackend.getInstance().loadDataFromFile(1, limit);
                    busyDialog.start();
                }
            }
        });
    }

    @Override
    public void setFocus() {
    }

    public void startTracing() {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                enableButtons(false);
            }
        });
    }

    public void finishLoadingFile(final TracingStatus status) {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                switch (status) {
                case OK:
                    treeViewer.refresh();
                    if (busyDialog != null)
                        busyDialog.finish();
                    break;
                case EMPTY:
                    if (busyDialog != null)
                        busyDialog.finish();
                    Status status = new Status(IStatus.WARNING, Activator.PLUGIN_ID, "Selected files contain no data to display", null);
                    StatusManager.getManager().handle(status, StatusManager.SHOW);
                    break;
                default:
                    break;
                }
                enableButtons(true);
            }
        });
    }

    public void finishLoadingTraces(TracingStatus status) {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                if (busyDialog != null)
                    busyDialog.finish();
            }
        });
    }

    public void clearTraceLists() {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                treeViewer.refresh();
            }
        });
    }
}
