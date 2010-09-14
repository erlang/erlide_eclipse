package org.ttb.integration.views;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;
import org.erlide.core.erlang.util.ErlangFunction;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.ui.util.ErlModelUtils;
import org.ttb.integration.TracingStatus;
import org.ttb.integration.TraceBackend;
import org.ttb.integration.mvc.controller.CollectedTracesContentProvider;
import org.ttb.integration.mvc.model.CollectedDataList;
import org.ttb.integration.mvc.model.ITraceNodeObserver;
import org.ttb.integration.mvc.model.TracePattern;
import org.ttb.integration.mvc.model.TracedNode;
import org.ttb.integration.mvc.model.treenodes.FunctionNode;
import org.ttb.integration.mvc.model.treenodes.ITreeNode;
import org.ttb.integration.mvc.model.treenodes.ModuleNode;
import org.ttb.integration.mvc.view.CollectedTracesLabelProvider;
import org.ttb.integration.ui.dialogs.BusyDialog;

/**
 * Sequence diagram which shows tracing results.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TreeViewerView extends ViewPart implements ITraceNodeObserver {

    private TreeViewer treeViewer;
    private Action clearAction;
    private Action loadAction;
    private BusyDialog busyDialog;

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
                // TODO add support for multiple selection
                FileDialog dialog = new FileDialog(PlatformUI.getWorkbench().getDisplay().getActiveShell(), SWT.OPEN);
                dialog.setFilterPath(ResourcesPlugin.getWorkspace().getRoot().getLocation().toString());
                // dialog.setFilterExtensions(new String[] { "*.*" });
                dialog.setText("Load trace data...");
                final String selected = dialog.open();
                if (selected != null) {
                    TraceBackend.getInstance().loadData(selected);
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
                CollectedDataList.getInstance().clear();
                treeViewer.setInput(CollectedDataList.getInstance());
            }
        };
        clearAction.setImageDescriptor(DebugUITools.getImageDescriptor(IDebugUIConstants.IMG_LCL_REMOVE_ALL));
        clearAction.setToolTipText("Clear view");

        manager.add(loadAction);
        manager.add(clearAction);
    }

    private void createTreeViewerPanel(Composite parent) {
        final Composite container = new Composite(parent, SWT.NONE);
        container.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        container.setLayout(new GridLayout());

        // treeViewer = new TreeViewer(container, SWT.VIRTUAL);
        treeViewer = new TreeViewer(container);
        treeViewer.getTree().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        // providers
        treeViewer.setContentProvider(new CollectedTracesContentProvider(treeViewer));
        treeViewer.setLabelProvider(new CollectedTracesLabelProvider());

        // input
        treeViewer.setInput(CollectedDataList.getInstance());

        // listener
        treeViewer.addDoubleClickListener(new IDoubleClickListener() {

            public void doubleClick(DoubleClickEvent event) {
                doDoubleClick(event);
            }
        });
    }

    /**
     * Action performed when user double-clicks on tree element
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

    public void addPattern(TracePattern tracePattern) {
    }

    public void removePattern(TracePattern tracePattern) {
    }

    public void updatePattern(TracePattern tracePattern) {
    }

    public void loadPatterns() {
    }

    public void startTracing() {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                loadAction.setEnabled(false);
                clearAction.setEnabled(false);
                treeViewer.refresh();
            }
        });
    }

    public void receivedTraceData() {
        // treeViewer.refresh();
    }

    public void startLoading() {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                loadAction.setEnabled(false);
                clearAction.setEnabled(false);
                treeViewer.refresh();
            }
        });
    }

    public void finishLoading(final TracingStatus status) {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                if (TracingStatus.OK.equals(status))
                    treeViewer.refresh();
                loadAction.setEnabled(true);
                clearAction.setEnabled(true);
                if (busyDialog != null)
                    busyDialog.finish();
            }
        });
    }

    public void addNode(TracedNode tracedNode) {
    }

    public void removeNode(TracedNode tracedNode) {
    }

    public void updateNode(TracedNode tracedNode) {
    }

    public void loadNodes() {
    }
}
