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
import org.erlide.backend.IBackend;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.jinterface.ErlLogger;
import org.erlide.launch.ErlLaunchAttributes;
import org.erlide.launch.debug.ErlDebugConstants;
import org.erlide.launch.debug.IErlangDebugNode;
import org.erlide.launch.debug.model.ErlangDebugElement;
import org.erlide.launch.debug.model.ErlangDebugTarget;
import org.erlide.launch.debug.model.ErtsProcess;
import org.erlide.ui.editors.util.EditorUtility;
import org.erlide.ui.launch.DebugTab;
import org.erlide.ui.launch.DebugTab.TreeContentProvider;
import org.erlide.ui.launch.DebugTreeItem;
import org.erlide.utils.SystemUtils;

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

    @Override
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
                ErlLogger.debug("no debug target found for " + selection);
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

    @Override
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
            if (erlangDebugTarget == null) {
                ErlLogger.warn("erlangDebugTarget is null ?!?!");
                return;
            }
            final Set<String> interpret = erlangDebugTarget
                    .getInterpretedModules();
            final Set<IErlModule> interpretedModules = new HashSet<IErlModule>(
                    interpret.size());
            DebugTab.addModules(interpret, interpretedModules);
            checkboxTreeViewer.getControl().getDisplay()
                    .syncExec(new Runnable() {
                        @Override
                        public void run() {
                            root.setChecked(checkboxTreeViewer,
                                    interpretedModules);
                        }
                    });
        }
    }

    @Override
    protected void configureToolBar(final IToolBarManager tbm) {
    }

    @Override
    protected void createActions() {
    }

    @Override
    protected Viewer createViewer(final Composite parent) {
        checkboxTreeViewer = new CheckboxTreeViewer(parent, SWT.BORDER);
        final ICheckStateListener checkStateListener = new ICheckStateListener() {
            @Override
            public void checkStateChanged(final CheckStateChangedEvent event) {
                final DebugTreeItem dti = (DebugTreeItem) event.getElement();
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

            @Override
            public void doubleClick(final DoubleClickEvent event) {
                final StructuredSelection ss = (StructuredSelection) event
                        .getSelection();
                final Object o = ss.getFirstElement();
                if (o instanceof DebugTreeItem) {
                    final DebugTreeItem item = (DebugTreeItem) o;
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
    }

    @Override
    protected String getHelpContextId() {
        return null;
    }

    @Override
    protected void becomesHidden() {
        setViewerInput(null);
        super.becomesHidden();
    }

    @Override
    protected void becomesVisible() {
        super.becomesVisible();
        final ISelection selection = DebugUITools.getDebugContextManager()
                .getContextService(getSite().getWorkbenchWindow())
                .getActiveContext();
        contextActivated(selection);
    }

    private void interpretOrDeinterpret(final DebugTreeItem dti,
            final boolean checked) {
        if (erlangDebugTarget == null) {
            ErlLogger.warn("erlangDebugTarget is null ?!?!");
            return;
        }
        final String module = dti.getItem().getName();
        final String moduleWoExtension = SystemUtils.withoutExtension(module);
        final IProject project = dti.getItem().getProject()
                .getWorkspaceProject();
        final boolean interpret = checked;
        final IBackend backend = erlangDebugTarget.getBackend();

        if (erlangDebugTarget.getInterpretedModules().contains(
                moduleWoExtension) != interpret) {
            // FIXME this isn't correct!!!
            backend.interpret(project, module, distributed, interpret);
        }
    }
}
