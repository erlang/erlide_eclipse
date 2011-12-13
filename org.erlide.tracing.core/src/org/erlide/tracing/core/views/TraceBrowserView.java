package org.erlide.tracing.core.views;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
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
import org.erlide.jinterface.ErlLogger;
import org.erlide.tracing.core.Activator;
import org.erlide.tracing.core.ITraceNodeObserver;
import org.erlide.tracing.core.TraceBackend;
import org.erlide.tracing.core.TracingStatus;
import org.erlide.tracing.core.mvc.model.TraceCollections;
import org.erlide.tracing.core.mvc.model.treenodes.ITreeNode;
import org.erlide.tracing.core.mvc.model.treenodes.TracingResultsNode;
import org.erlide.tracing.core.mvc.view.TreeContentProvider;
import org.erlide.tracing.core.mvc.view.TreeLabelProvider;
import org.erlide.tracing.core.preferences.PreferenceNames;
import org.erlide.tracing.core.ui.dialogs.RunnableWithProgress;
import org.erlide.tracing.core.utils.TracingStatusHandler;

/**
 * Sequence diagram which shows tracing results.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TraceBrowserView extends ViewPart implements ITraceNodeObserver {

    private TreeViewer treeViewer;
    private Action removeAllAction;
    private Action loadAction;
    private Action removeAction;
    private RunnableWithProgress task;
    private TracingStatus status;

    public TraceBrowserView() {
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

        // toolbars and menu
        createActionBars();

        // children
        createTreeViewerPanel(parent);

        // enable/disable buttons depending on whether tracing is started or not
        enableActions(!TraceBackend.getInstance().isStarted());
    }

    private void createActionBars() {
        loadAction = new Action() {
            @Override
            public void run() {
                final DirectoryDialog dialog = new DirectoryDialog(PlatformUI
                        .getWorkbench().getDisplay().getActiveShell(), SWT.OPEN);
                dialog.setFilterPath(ResourcesPlugin.getWorkspace().getRoot()
                        .getLocation().toString());
                // dialog.setFilterExtensions(new String[] { "*.*" });
                dialog.setText("Load trace data...");
                final String selected = dialog.open();
                if (selected != null) {
                    task = new RunnableWithProgress("Load trace data...") {
                        @Override
                        public void doAction() {
                            TraceBackend.getInstance().loadFile(selected);
                        }
                    };
                    try {
                        final Shell shell = PlatformUI.getWorkbench()
                                .getActiveWorkbenchWindow().getShell();
                        new ProgressMonitorDialog(shell).run(true, false, task);
                        doAfterLoadingFile();
                    } catch (final Exception e) {
                        ErlLogger.error(e);
                    } finally {
                        task = null;
                    }
                }
            }
        };
        loadAction.setImageDescriptor(PlatformUI.getWorkbench()
                .getSharedImages()
                .getImageDescriptor(ISharedImages.IMG_OBJ_FOLDER));
        loadAction.setToolTipText("Load results from disk...");

        removeAction = new Action() {
            @Override
            public void run() {
                final TracingResultsNode node = (TracingResultsNode) ((IStructuredSelection) treeViewer
                        .getSelection()).getFirstElement();
                if (node != null) {
                    TraceBackend.getInstance().removeTracingResult(node);
                }
            }
        };
        removeAction.setImageDescriptor(DebugUITools
                .getImageDescriptor(IDebugUIConstants.IMG_LCL_REMOVE));
        removeAction.setToolTipText("Remove selected");

        removeAllAction = new Action() {
            @Override
            public void run() {
                TraceBackend.getInstance().clearTraceLists();
            }
        };
        removeAllAction.setImageDescriptor(DebugUITools
                .getImageDescriptor(IDebugUIConstants.IMG_LCL_REMOVE_ALL));
        removeAllAction.setToolTipText("Remove all");

        final IToolBarManager manager = getViewSite().getActionBars()
                .getToolBarManager();
        manager.add(loadAction);
        manager.add(removeAction);
        manager.add(removeAllAction);
    }

    private void enableActions(final boolean enabled) {
        loadAction.setEnabled(enabled);
        removeAction.setEnabled(enabled);
        removeAllAction.setEnabled(enabled);
        treeViewer.getTree().setEnabled(enabled);
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
                .setContentProvider(new TreeContentProvider(treeViewer, false));
        treeViewer.setLabelProvider(new TreeLabelProvider());

        // input
        treeViewer.setInput(TraceCollections.getFilesList());

        // listener
        treeViewer.addSelectionChangedListener(new ISelectionChangedListener() {

            @Override
            public void selectionChanged(final SelectionChangedEvent event) {
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
        final IStructuredSelection selection = (IStructuredSelection) event
                .getSelection();
        final ITreeNode treeNode = (ITreeNode) selection.getFirstElement();
        if (treeNode != null) {
            task = new RunnableWithProgress("Loading trace results...") {
                @Override
                public void doAction() {
                    TraceBackend.getInstance().setActiveResultSet(
                            (TracingResultsNode) treeNode);
                    final int limit = Activator.getDefault()
                            .getPreferenceStore()
                            .getInt(PreferenceNames.TRACES_LOAD_LIMIT);
                    TraceBackend.getInstance().loadDataFromFile(1, limit);
                }
            };
            try {
                final Shell shell = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getShell();
                new ProgressMonitorDialog(shell).run(true, false, task);
                doAfterLoadingFile();
            } catch (final Exception e) {
                ErlLogger.error(e);
            } finally {
                task = null;
            }
        }
    }

    private void doAfterLoadingFile() {
        if (TracingStatus.OK.equals(status)) {
            treeViewer.refresh();
        }
        if (task != null) {
            // task was executed from this class so this class is responsible
            // for handling status
            TracingStatusHandler.handleStatus(status);
        }
        enableActions(true);
    }

    @Override
    public void setFocus() {
    }

    @Override
    public void startTracing() {
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                enableActions(false);
            }
        });
    }

    @Override
    public void finishLoadingFile(final TracingStatus theStatus) {
        status = theStatus;
        if (task != null) {
            // when loading was initialized from this view
            task.finish();
        } else {
            // when loading was initialized outside this view
            Display.getDefault().asyncExec(new Runnable() {
                @Override
                public void run() {
                    doAfterLoadingFile();
                }
            });
        }
    }

    @Override
    public void finishLoadingTraces(final TracingStatus theStatus) {
        status = theStatus;
        if (task != null) {
            task.finish();
        }
    }

    @Override
    public void removeFile() {
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                treeViewer.refresh();
            }
        });
    }

    @Override
    public void updateTracePatterns() {
    }
}
