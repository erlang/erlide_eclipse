/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.console;

import java.io.IOException;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IDebugEventSetListener;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.eclipse.debug.core.model.IStreamsProxy2;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.debug.ui.contexts.DebugContextEvent;
import org.eclipse.debug.ui.contexts.IDebugContextListener;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleConstants;
import org.eclipse.ui.console.IConsolePageParticipant;
import org.eclipse.ui.console.IConsoleView;
import org.eclipse.ui.contexts.IContextActivation;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.handlers.IHandlerActivation;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.part.IPageBookViewPage;
import org.eclipse.ui.part.IPageSite;
import org.eclipse.ui.part.IShowInSource;
import org.eclipse.ui.part.IShowInTargetList;
import org.eclipse.ui.part.ShowInContext;
import org.erlide.ui.console.actions.ConsoleRemoveAllTerminatedAction;
import org.erlide.ui.console.actions.ConsoleRemoveLaunchAction;
import org.erlide.ui.console.actions.ConsoleTerminateAction;
import org.erlide.ui.console.actions.ShowStandardOutAction;
import org.erlide.ui.console.actions.ShowWhenContentChangesAction;
import org.erlide.ui.internal.ErlideUIPlugin;

/**
 * Creates and manages process console specific actions
 * 
 * @since 3.1
 */
public class ConsolePageParticipant implements IConsolePageParticipant,
        IShowInSource, IShowInTargetList, IDebugEventSetListener,
        IDebugContextListener {

    // actions
    private ConsoleTerminateAction fTerminate;
    private ConsoleRemoveLaunchAction fRemoveTerminated;
    private ConsoleRemoveAllTerminatedAction fRemoveAllTerminated;
    private ShowWhenContentChangesAction fStdOut;

    private ErlangConsole fConsole;
    private IPageBookViewPage fPage;
    private IConsoleView fView;

    private EOFHandler fEOFHandler;
    private final String fContextId = "org.eclipse.debug.ui.console"; //$NON-NLS-1$;
    private IContextActivation fActivatedContext;
    private IHandlerActivation fActivatedHandler;

    /**
     * Handler to send EOF
     */
    private class EOFHandler extends AbstractHandler {
        @Override
        public Object execute(final ExecutionEvent event)
                throws org.eclipse.core.commands.ExecutionException {
            final IStreamsProxy proxy = getProcess().getStreamsProxy();
            if (proxy instanceof IStreamsProxy2) {
                final IStreamsProxy2 proxy2 = (IStreamsProxy2) proxy;
                try {
                    proxy2.closeInputStream();
                } catch (final IOException e1) {
                }
            }
            return null;
        }

    }

    @Override
    public void init(final IPageBookViewPage page, final IConsole console) {
        fPage = page;
        fConsole = (ErlangConsole) console;

        fTerminate = new ConsoleTerminateAction(fConsole);
        fRemoveTerminated = new ConsoleRemoveLaunchAction(fConsole);
        fRemoveAllTerminated = new ConsoleRemoveAllTerminatedAction();
        fStdOut = new ShowStandardOutAction();

        fView = (IConsoleView) fPage.getSite().getPage()
                .findView(IConsoleConstants.ID_CONSOLE_VIEW);

        fTerminate.update();

        DebugPlugin.getDefault().addDebugEventListener(this);
        DebugUITools.getDebugContextManager()
                .getContextService(fPage.getSite().getWorkbenchWindow())
                .addDebugContextListener(this);

        // contribute to toolbar
        final IActionBars actionBars = fPage.getSite().getActionBars();
        configureToolBar(actionBars.getToolBarManager());

        // create handler and submissions for EOF
        fEOFHandler = new EOFHandler();

        // set global ref, used by the SendToConsole action
        // FIXME global is bad, use project's page (keyed by backend?)
        ErlideUIPlugin.getDefault().setConsolePage((ErlangConsolePage) fPage);
    }

    @Override
    public void dispose() {
        DebugUITools.getDebugContextManager()
                .getContextService(fPage.getSite().getWorkbenchWindow())
                .removeDebugContextListener(this);
        DebugPlugin.getDefault().removeDebugEventListener(this);
        if (fRemoveTerminated != null) {
            fRemoveTerminated.dispose();
            fRemoveTerminated = null;
        }
        if (fRemoveAllTerminated != null) {
            fRemoveAllTerminated.dispose();
            fRemoveAllTerminated = null;
        }
        if (fTerminate != null) {
            fTerminate.dispose();
            fTerminate = null;
        }
        if (fStdOut != null) {
            fStdOut.dispose();
            fStdOut = null;
        }
        fConsole = null;
        if (ErlideUIPlugin.getDefault().getConsolePage() == fPage) {
            ErlideUIPlugin.getDefault().setConsolePage(null);
        }
    }

    /**
     * Contribute actions to the toolbar
     */
    protected void configureToolBar(final IToolBarManager mgr) {
        mgr.appendToGroup(IConsoleConstants.LAUNCH_GROUP, fTerminate);
        mgr.appendToGroup(IConsoleConstants.LAUNCH_GROUP, fRemoveTerminated);
        mgr.appendToGroup(IConsoleConstants.LAUNCH_GROUP, fRemoveAllTerminated);
        mgr.appendToGroup(IConsoleConstants.OUTPUT_GROUP, fStdOut);
    }

    @Override
    public Object getAdapter(@SuppressWarnings("rawtypes") final Class required) {
        if (IShowInSource.class.equals(required)) {
            return this;
        }
        if (IShowInTargetList.class.equals(required)) {
            return this;
        }
        // CONTEXTLAUNCHING
        if (ILaunchConfiguration.class.equals(required)) {
            final ILaunch launch = getProcess().getLaunch();
            if (launch != null) {
                return launch.getLaunchConfiguration();
            }
            return null;
        }
        return null;
    }

    @Override
    public ShowInContext getShowInContext() {
        final IProcess process = getProcess();
        if (process == null) {
            return null;
        }
        final IDebugTarget target = (IDebugTarget) process
                .getAdapter(IDebugTarget.class);
        ISelection selection = null;
        if (target == null) {
            selection = new TreeSelection(new TreePath(new Object[] {
                    DebugPlugin.getDefault().getLaunchManager(),
                    process.getLaunch(), process }));
        } else {
            selection = new TreeSelection(new TreePath(new Object[] {
                    DebugPlugin.getDefault().getLaunchManager(),
                    target.getLaunch(), target }));
        }
        return new ShowInContext(null, selection);
    }

    @Override
    public String[] getShowInTargetIds() {
        return new String[] { IDebugUIConstants.ID_DEBUG_VIEW };
    }

    @Override
    public void handleDebugEvents(final DebugEvent[] events) {
        for (int i = 0; i < events.length; i++) {
            final DebugEvent event = events[i];
            if (event.getSource().equals(getProcess())) {
                final Runnable r = new Runnable() {
                    @Override
                    public void run() {
                        if (fTerminate != null) {
                            fTerminate.update();
                        }
                    }
                };
                PlatformUI.getWorkbench().getDisplay().asyncExec(r);
            }
        }
    }

    protected IProcess getProcess() {
        return null;
        // return fConsole != null ? fConsole.getProcess() : null;
    }

    @Override
    public void activated() {
        // add EOF submissions
        final IPageSite site = fPage.getSite();
        final IHandlerService handlerService = (IHandlerService) site
                .getService(IHandlerService.class);
        final IContextService contextService = (IContextService) site
                .getService(IContextService.class);
        fActivatedContext = contextService.activateContext(fContextId);
        fActivatedHandler = handlerService.activateHandler(
                "org.eclipse.debug.ui.commands.eof", fEOFHandler); //$NON-NLS-1$
        ErlideUIPlugin.getDefault().setConsolePage((ErlangConsolePage) fPage);
    }

    @Override
    public void deactivated() {
        // remove EOF submissions
        final IPageSite site = fPage.getSite();
        final IHandlerService handlerService = (IHandlerService) site
                .getService(IHandlerService.class);
        final IContextService contextService = (IContextService) site
                .getService(IContextService.class);
        handlerService.deactivateHandler(fActivatedHandler);
        contextService.deactivateContext(fActivatedContext);
    }

    @Override
    public void debugContextChanged(final DebugContextEvent event) {
        if ((event.getFlags() & DebugContextEvent.ACTIVATED) > 0) {
            final IProcess process = getProcess();
            if (fView != null && process != null
                    && process.equals(DebugUITools.getCurrentProcess())) {
                fView.display(fConsole);
            }
        }

    }
}
