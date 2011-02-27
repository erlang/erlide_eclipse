package org.erlide.debug.ui.views;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IDebugEventSetListener;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.ui.AbstractDebugView;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.contexts.DebugContextEvent;
import org.eclipse.debug.ui.contexts.IDebugContextListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PartInitException;
import org.erlide.core.backend.ErlDebugConstants;
import org.erlide.core.backend.ErlLaunchAttributes;
import org.erlide.core.backend.ErtsProcess;
import org.erlide.core.backend.RpcCallSite;
import org.erlide.core.common.CommonUtils;
import org.erlide.core.model.debug.ErlangDebugElement;
import org.erlide.core.model.debug.ErlangDebugHelper;
import org.erlide.core.model.debug.ErlangDebugTarget;
import org.erlide.core.model.debug.IErlangDebugNode;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.ui.editors.util.EditorUtility;
import org.erlide.ui.launch.DebugTab;
import org.erlide.ui.launch.DebugTab.DebugTreeItem;
import org.erlide.ui.launch.DebugTab.TreeContentProvider;

/**
 * A view with a checkbox tree of interpreted modules checking/unchecking
 * interpretes/de-interpretes a module
 * 
 * @author jakob
 */
public class InterpretedModulesView extends AbstractDebugView implements
        IDebugEventSetListener, IDebugContextListener {

    CheckboxTreeViewer checkboxTreeViewer;
    private ErlangDebugTarget erlangDebugTarget;
    private boolean distributed;

    public void debugContextChanged(final DebugContextEvent event) {
        if ((event.getFlags() & DebugContextEvent.ACTIVATED) > 0) {
            contextActivated(event.getContext());
        }
    }

    private void contextActivated(final ISelection selection) {
        if (!isAvailable() || !isVisible()) {
            return;
        }
        final TreeContentProvider contentProvider = (TreeContentProvider) checkboxTreeViewer
                .getContentProvider();
        contentProvider.setRoot(new DebugTreeItem(null, null));
        erlangDebugTarget = null;
        final List<IErlModule> interpretedModules = new ArrayList<IErlModule>();
        if (selection instanceof IStructuredSelection) {
            final IStructuredSelection structuredSelection = (IStructuredSelection) selection;
            final Object o = structuredSelection.getFirstElement();
            if (o instanceof ErlangDebugElement) {
                final ErlangDebugElement e = (ErlangDebugElement) o;
                erlangDebugTarget = e.getErlangDebugTarget();
            } else if (o instanceof ILaunch) {
                final ILaunch launch = (ILaunch) o;
                final IDebugTarget target = launch.getDebugTarget();
                if (target instanceof IErlangDebugNode) {
                    final IErlangDebugNode edn = (IErlangDebugNode) target;
                    erlangDebugTarget = edn.getErlangDebugTarget();
                }
            } else if (o instanceof ErtsProcess) {
                final ErtsProcess ep = (ErtsProcess) o;
                final ILaunch launch = ep.getLaunch();
                final IDebugTarget target = launch.getDebugTarget();
                if (target instanceof IErlangDebugNode) {
                    final IErlangDebugNode edn = (IErlangDebugNode) target;
                    erlangDebugTarget = edn.getErlangDebugTarget();
                }
            }
            if (erlangDebugTarget == null) {
                return;
            }
            final ILaunchConfiguration launchConfiguration = erlangDebugTarget
                    .getLaunch().getLaunchConfiguration();
            setViewerInput(launchConfiguration);
            checkboxTreeViewer.expandAll();
            try {
                final int debugFlags = launchConfiguration.getAttribute(
                        ErlLaunchAttributes.DEBUG_FLAGS,
                        ErlDebugConstants.DEFAULT_DEBUG_FLAGS);
                distributed = (debugFlags & ErlDebugConstants.DISTRIBUTED_DEBUG) != 0;
            } catch (final CoreException e1) {
                distributed = false;
            }
            DebugTab.addModules(erlangDebugTarget.getInterpretedModules(),
                    interpretedModules);
        }
        checkboxTreeViewer.refresh();
        final DebugTreeItem root = contentProvider.getRoot();
        if (root != null) {
            root.setChecked(checkboxTreeViewer, interpretedModules);
        }
        showViewer();

        // updateAction(VARIABLES_FIND_ELEMENT_ACTION);
        // updateAction(FIND_ACTION);
    }

    @SuppressWarnings("unchecked")
    private void setViewerInput(final ILaunchConfiguration launchConfiguration) {
        checkboxTreeViewer.setInput(launchConfiguration);
        if (launchConfiguration != null) {
            List<String> interpret;
            try {
                interpret = launchConfiguration.getAttribute(
                        ErlLaunchAttributes.DEBUG_INTERPRET_MODULES,
                        new ArrayList<String>());
            } catch (final CoreException e1) {
                interpret = new ArrayList<String>();
            }
            final ArrayList<IErlModule> interpretedModules = new ArrayList<IErlModule>(
                    interpret.size());
            DebugTab.addModules(interpret, interpretedModules);
        }
    }

    public void handleDebugEvents(final DebugEvent[] events) {
        boolean changed = false;
        for (final DebugEvent debugEvent : events) {
            if (debugEvent.getKind() == DebugEvent.MODEL_SPECIFIC
                    && debugEvent.getDetail() == ErlangDebugTarget.INTERPRETED_MODULES_CHANGED) {
                changed = true;
                break;
            }
        }
        if (changed) {
            final DebugTreeItem root = ((TreeContentProvider) checkboxTreeViewer
                    .getContentProvider()).getRoot();
            if (root == null) {
                return;
            }
            final Set<String> interpret = erlangDebugTarget
                    .getInterpretedModules();
            final Set<IErlModule> interpretedModules = new HashSet<IErlModule>(
                    interpret.size());
            DebugTab.addModules(interpret, interpretedModules);
            checkboxTreeViewer.getControl().getDisplay()
                    .syncExec(new Runnable() {
                        public void run() {
                            root.setChecked(checkboxTreeViewer,
                                    interpretedModules);
                        }
                    });
        }
    }

    @Override
    protected void configureToolBar(final IToolBarManager tbm) {
        // TODO Auto-generated method stub

    }

    @Override
    protected void createActions() {
        // TODO Auto-generated method stub

    }

    @Override
    protected Viewer createViewer(final Composite parent) {
        checkboxTreeViewer = new CheckboxTreeViewer(parent, SWT.BORDER);
        final ICheckStateListener checkStateListener = new ICheckStateListener() {
            public void checkStateChanged(final CheckStateChangedEvent event) {
                final DebugTab.DebugTreeItem dti = (DebugTreeItem) event
                        .getElement();
                checkboxTreeViewer.setGrayed(dti, false);
                final boolean checked = event.getChecked();
                setSubtreeChecked(dti, checked);
                DebugTab.checkUpwards(checkboxTreeViewer, dti, checked, false);
            }

        };
        checkboxTreeViewer.addCheckStateListener(checkStateListener);
        checkboxTreeViewer.setLabelProvider(new DebugTab.TreeLabelProvider());
        checkboxTreeViewer
                .setContentProvider(new DebugTab.TreeContentProvider());
        checkboxTreeViewer.addDoubleClickListener(new IDoubleClickListener() {

            public void doubleClick(final DoubleClickEvent event) {
                final StructuredSelection ss = (StructuredSelection) event
                        .getSelection();
                final Object o = ss.getFirstElement();
                if (o instanceof DebugTab.DebugTreeItem) {
                    final DebugTab.DebugTreeItem item = (DebugTab.DebugTreeItem) o;
                    try {
                        EditorUtility.openInEditor(item.getItem());
                    } catch (final PartInitException e) {
                    }
                }
            }

        });
        DebugUITools.getDebugContextManager().addDebugContextListener(this);
        DebugPlugin.getDefault().addDebugEventListener(this);
        return checkboxTreeViewer;
    }

    protected void setSubtreeChecked(final DebugTreeItem dti,
            final boolean checked) {
        final List<DebugTreeItem> children = dti.getChildren();
        if (children == null || children.size() == 0) {
            interpretOrDeinterpret(dti, checked);
        } else {
            for (final DebugTreeItem i : children) {
                checkboxTreeViewer.setChecked(i, checked);
                setSubtreeChecked(i, checked);
            }
        }
    }

    @Override
    protected void fillContextMenu(final IMenuManager menu) {
        // TODO Auto-generated method stub
    }

    @Override
    protected String getHelpContextId() {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.debug.ui.AbstractDebugView#becomesHidden()
     */
    @Override
    protected void becomesHidden() {
        setViewerInput(null);
        super.becomesHidden();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.debug.ui.AbstractDebugView#becomesVisible()
     */
    @Override
    protected void becomesVisible() {
        super.becomesVisible();
        final ISelection selection = DebugUITools.getDebugContextManager()
                .getContextService(getSite().getWorkbenchWindow())
                .getActiveContext();
        contextActivated(selection);
    }

    private void interpretOrDeinterpret(final DebugTab.DebugTreeItem dti,
            final boolean checked) {
        final String module = dti.getItem().getName();
        final String moduleWoExtension = CommonUtils.withoutExtension(module);
        final IProject project = dti.getItem().getErlProject().getProject();
        final boolean interpret = checked;
        final RpcCallSite backend = erlangDebugTarget.getBackend();

        if (erlangDebugTarget.getInterpretedModules().contains(
                moduleWoExtension) != interpret) {
            // FIXME this isn't correct!!!
            new ErlangDebugHelper().interpret(backend, project, module,
                    distributed, interpret);
        }
    }
}
