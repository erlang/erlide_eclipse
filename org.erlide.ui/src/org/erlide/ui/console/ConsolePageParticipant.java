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
		public Object execute(ExecutionEvent event)
				throws org.eclipse.core.commands.ExecutionException {
			IStreamsProxy proxy = getProcess().getStreamsProxy();
			if (proxy instanceof IStreamsProxy2) {
				IStreamsProxy2 proxy2 = (IStreamsProxy2) proxy;
				try {
					proxy2.closeInputStream();
				} catch (IOException e1) {
				}
			}
			return null;
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.console.IConsolePageParticipant#init(IPageBookViewPage,
	 * IConsole)
	 */
	public void init(IPageBookViewPage page, IConsole console) {
		fPage = page;
		fConsole = (ErlangConsole) console;

		fRemoveTerminated = new ConsoleRemoveLaunchAction(fConsole.getBackend());
		fRemoveAllTerminated = new ConsoleRemoveAllTerminatedAction();
		fTerminate = new ConsoleTerminateAction(fConsole);
		fStdOut = new ShowStandardOutAction();

		fView = (IConsoleView) fPage.getSite().getPage().findView(
				IConsoleConstants.ID_CONSOLE_VIEW);

		DebugPlugin.getDefault().addDebugEventListener(this);
		DebugUITools.getDebugContextManager().getContextService(
				fPage.getSite().getWorkbenchWindow()).addDebugContextListener(
				this);

		// contribute to toolbar
		IActionBars actionBars = fPage.getSite().getActionBars();
		configureToolBar(actionBars.getToolBarManager());

		// create handler and submissions for EOF
		fEOFHandler = new EOFHandler();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.console.IConsolePageParticipant#dispose()
	 */
	public void dispose() {
		DebugUITools.getDebugContextManager().getContextService(
				fPage.getSite().getWorkbenchWindow())
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
	}

	/**
	 * Contribute actions to the toolbar
	 */
	protected void configureToolBar(IToolBarManager mgr) {
		mgr.appendToGroup(IConsoleConstants.LAUNCH_GROUP, fTerminate);
		mgr.appendToGroup(IConsoleConstants.LAUNCH_GROUP, fRemoveTerminated);
		mgr.appendToGroup(IConsoleConstants.LAUNCH_GROUP, fRemoveAllTerminated);
		mgr.appendToGroup(IConsoleConstants.OUTPUT_GROUP, fStdOut);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
	 */
	@SuppressWarnings("unchecked")
	public Object getAdapter(Class required) {
		if (IShowInSource.class.equals(required)) {
			return this;
		}
		if (IShowInTargetList.class.equals(required)) {
			return this;
		}
		// CONTEXTLAUNCHING
		if (ILaunchConfiguration.class.equals(required)) {
			ILaunch launch = getProcess().getLaunch();
			if (launch != null) {
				return launch.getLaunchConfiguration();
			}
			return null;
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.part.IShowInSource#getShowInContext()
	 */
	public ShowInContext getShowInContext() {
		IProcess process = getProcess();
		if (process == null) {
			return null;
		}
		IDebugTarget target = (IDebugTarget) process
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

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.part.IShowInTargetList#getShowInTargetIds()
	 */
	public String[] getShowInTargetIds() {
		return new String[] { IDebugUIConstants.ID_DEBUG_VIEW };
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.debug.core.IDebugEventSetListener#handleDebugEvents(org.eclipse
	 * .debug.core.DebugEvent[])
	 */
	public void handleDebugEvents(DebugEvent[] events) {
		for (int i = 0; i < events.length; i++) {
			DebugEvent event = events[i];
			if (event.getSource().equals(getProcess())) {
				Runnable r = new Runnable() {
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

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.console.IConsolePageParticipant#activated()
	 */
	public void activated() {
		// add EOF submissions
		IPageSite site = fPage.getSite();
		IHandlerService handlerService = (IHandlerService) site
				.getService(IHandlerService.class);
		IContextService contextService = (IContextService) site
				.getService(IContextService.class);
		fActivatedContext = contextService.activateContext(fContextId);
		fActivatedHandler = handlerService.activateHandler(
				"org.eclipse.debug.ui.commands.eof", fEOFHandler); //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.console.IConsolePageParticipant#deactivated()
	 */
	public void deactivated() {
		// remove EOF submissions
		IPageSite site = fPage.getSite();
		IHandlerService handlerService = (IHandlerService) site
				.getService(IHandlerService.class);
		IContextService contextService = (IContextService) site
				.getService(IContextService.class);
		handlerService.deactivateHandler(fActivatedHandler);
		contextService.deactivateContext(fActivatedContext);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.debug.internal.ui.contexts.provisional.IDebugContextListener
	 * #contextEvent
	 * (org.eclipse.debug.internal.ui.contexts.provisional.DebugContextEvent)
	 */
	public void debugContextChanged(DebugContextEvent event) {
		if ((event.getFlags() & DebugContextEvent.ACTIVATED) > 0) {
			IProcess process = getProcess();
			if (fView != null && process != null
					&& process.equals(DebugUITools.getCurrentProcess())) {
				fView.display(fConsole);
			}
		}

	}
}
