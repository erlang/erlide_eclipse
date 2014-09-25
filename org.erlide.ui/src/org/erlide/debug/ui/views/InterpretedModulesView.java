package org.erlide.debug.ui.views;

import java.util.EnumSet;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IDebugEventSetListener;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.RuntimeProcess;
import org.eclipse.debug.ui.AbstractDebugView;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.contexts.DebugContextEvent;
import org.eclipse.debug.ui.contexts.IDebugContextListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PartInitException;
import org.erlide.backend.api.ErlRuntimeAttributes;
import org.erlide.backend.debug.IErlangDebugNode;
import org.erlide.backend.debug.model.ErlangDebugElement;
import org.erlide.backend.debug.model.ErlangDebugTarget;
import org.erlide.debug.ui.utils.ModuleItemLabelProvider;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.runtime.api.ErlDebugFlags;
import org.erlide.ui.editors.util.EditorUtility;
import org.erlide.util.ErlLogger;

/**
 * A view with a checkbox tree of interpreted modules checking/unchecking
 * interpretes/de-interpretes a module
 *
 * @author jakob
 */
public class InterpretedModulesView extends AbstractDebugView implements
        IDebugEventSetListener, IDebugContextListener {

    ListViewer listViewer;
    InterpretedModuleListContentProvider contentProvider;
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
        erlangDebugTarget = null;
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
            } else if (o instanceof RuntimeProcess) {
                final RuntimeProcess ep = (RuntimeProcess) o;
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
            try {
                final EnumSet<ErlDebugFlags> debugFlags = ErlDebugFlags
                        .makeSet(launchConfiguration.getAttribute(
                                ErlRuntimeAttributes.DEBUG_FLAGS,
                                ErlDebugFlags.getFlag(ErlDebugFlags.DEFAULT_DEBUG_FLAGS)));
                distributed = debugFlags.contains(ErlDebugFlags.DISTRIBUTED_DEBUG);
            } catch (final CoreException e1) {
                distributed = false;
            }
        }
        listViewer.refresh();
        showViewer();

        // updateAction(VARIABLES_FIND_ELEMENT_ACTION);
        // updateAction(FIND_ACTION);
    }

    private void setViewerInput(final ILaunchConfiguration launchConfiguration) {
        listViewer.setInput(launchConfiguration);
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
            if (erlangDebugTarget == null) {
                ErlLogger.warn("erlangDebugTarget is null ?!?!");
                return;
            }
            final Set<String> interpret = erlangDebugTarget.getInterpretedModules();
            contentProvider.setModules(interpret);
            refreshList();
        }
    }

    private void refreshList() {
        getSite().getShell().getDisplay().asyncExec(new Runnable() {

            @Override
            public void run() {
                listViewer.refresh();
            }
        });
    }

    @Override
    protected void configureToolBar(final IToolBarManager tbm) {
    }

    @Override
    protected void createActions() {

    }

    @Override
    protected Viewer createViewer(final Composite parent) {
        listViewer = new ListViewer(parent, SWT.BORDER);
        listViewer.setLabelProvider(new ModuleItemLabelProvider());
        contentProvider = new InterpretedModuleListContentProvider();
        listViewer.setContentProvider(contentProvider);
        getSite().setSelectionProvider(listViewer);
        listViewer.addDoubleClickListener(new IDoubleClickListener() {

            @Override
            public void doubleClick(final DoubleClickEvent event) {
                final IStructuredSelection ss = (IStructuredSelection) event
                        .getSelection();
                for (final Object o : ss.toArray()) {
                    try {
                        EditorUtility.openInEditor(o);
                    } catch (final PartInitException e) {
                        ErlLogger.warn(e);
                    }
                }
            }

        });
        DebugUITools.getDebugContextManager().addDebugContextListener(this);
        DebugPlugin.getDefault().addDebugEventListener(this);
        return listViewer;
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
                .getContextService(getSite().getWorkbenchWindow()).getActiveContext();
        contextActivated(selection);
    }

    public void interpretOrDeinterpret(final IErlModule module, final boolean checked) {
        if (erlangDebugTarget == null) {
            ErlLogger.warn("erlangDebugTarget is null ?!?!");
            return;
        }
        final String moduleWoExtension = module.getModuleName();
        final IProject project = ErlangEngine.getInstance().getModelUtilService()
                .getProject(module).getWorkspaceProject();
        final boolean interpret = checked;

        if (erlangDebugTarget.getInterpretedModules().contains(moduleWoExtension) != interpret) {
            // FIXME this isn't correct!!!
            erlangDebugTarget.interpret(project, moduleWoExtension, distributed,
                    interpret);
        }
        addRemove(module, checked);
    }

    private void addRemove(final IErlModule module, final boolean add) {
        if (add) {
            contentProvider.addModule(module);
        } else {
            contentProvider.removeModule(module);
        }
        refreshList();
    }
}
