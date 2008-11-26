/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Sebastian Davids <sdavids@gmx.de> - Images for menu items (27481)
 *     Eric Merritt - Api changes to support Erlide
 *******************************************************************************/
package org.erlide.ui.views.modules.actions;

import java.util.Iterator;

import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.actions.BuildAction;
import org.eclipse.ui.actions.CloseResourceAction;
import org.eclipse.ui.actions.OpenResourceAction;
import org.eclipse.ui.actions.RefreshAction;
import org.eclipse.ui.ide.IDEActionFactory;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.ErlideUIConstants;
import org.erlide.ui.views.modules.ModuleNavigator;

/**
 * This is the action group for workspace actions such as Build, Refresh Local,
 * and Open/Close Project.
 */
public class WorkspaceActionGroup extends ModuleNavigatorActionGroup {

	private BuildAction buildAction;

	private BuildAction rebuildAction;

	private OpenResourceAction openProjectAction;

	private CloseResourceAction closeProjectAction;

	private RefreshAction refreshAction;

	/**
	 * Simple constructor
	 * 
	 * @param lnavigator
	 */
	public WorkspaceActionGroup(ModuleNavigator lnavigator) {
		super(lnavigator);
	}

	/**
	 * fill the action bars
	 * 
	 * @see org.eclipse.ui.actions.ActionGroup#fillActionBars(org.eclipse.ui.IActionBars)
	 */
	@Override
	public void fillActionBars(IActionBars actionBars) {
		actionBars.setGlobalActionHandler(ActionFactory.REFRESH.getId(),
				refreshAction);
		actionBars.setGlobalActionHandler(IDEActionFactory.BUILD_PROJECT
				.getId(), buildAction);
		actionBars.setGlobalActionHandler(
				IDEActionFactory.OPEN_PROJECT.getId(), openProjectAction);
		actionBars.setGlobalActionHandler(IDEActionFactory.CLOSE_PROJECT
				.getId(), closeProjectAction);
	}

	/**
	 * Adds the build, open project, close project and refresh resource actions
	 * to the context menu.
	 * <p>
	 * The following conditions apply: build-only projects selected, auto build
	 * disabled, at least one builder present open project-only projects
	 * selected, at least one closed project close project-only projects
	 * selected, at least one open project refresh-no closed project selected
	 * </p>
	 * <p>
	 * Both the open project and close project action may be on the menu at the
	 * same time.
	 * </p>
	 * <p>
	 * No disabled action should be on the context menu.
	 * </p>
	 * 
	 * @param menu
	 *            context menu to add actions to
	 */
	@Override
	public void fillContextMenu(IMenuManager menu) {
		final IStructuredSelection selection = (IStructuredSelection) getContext()
				.getSelection();
		boolean isProjectSelection = true;
		boolean hasOpenProjects = false;
		boolean hasClosedProjects = false;
		boolean hasBuilder = true; // false if any project is closed or does
		// not
		// have builder
		final Iterator<?> resources = selection.iterator();

		while (resources.hasNext()
				&& (!hasOpenProjects || !hasClosedProjects || hasBuilder || isProjectSelection)) {
			final Object next = resources.next();
			IProject project = null;

			if (next instanceof IProject) {
				project = (IProject) next;
			} else if (next instanceof IAdaptable) {
				project = (IProject) ((IAdaptable) next)
						.getAdapter(IProject.class);
			}

			if (project == null) {
				isProjectSelection = false;
				continue;
			}
			if (project.isOpen()) {
				hasOpenProjects = true;
				if (hasBuilder && !hasBuilder(project)) {
					hasBuilder = false;
				}
			} else {
				hasClosedProjects = true;
				hasBuilder = false;
			}
		}
		if (!selection.isEmpty() && isProjectSelection
				&& !ResourcesPlugin.getWorkspace().isAutoBuilding()
				&& hasBuilder) {
			// Allow manual incremental build only if auto build is off.
			buildAction.selectionChanged(selection);
			menu.add(buildAction);
		}
		if (!hasClosedProjects) {
			refreshAction.selectionChanged(selection);
			menu.add(refreshAction);
		}
		if (isProjectSelection) {
			if (hasClosedProjects) {
				openProjectAction.selectionChanged(selection);
				menu.add(openProjectAction);
			}
			if (hasOpenProjects) {
				closeProjectAction.selectionChanged(selection);
				menu.add(closeProjectAction);
			}
		}
	}

	/**
	 * Handles a key pressed event by invoking the appropriate action.
	 * 
	 * @param event
	 *            The key event
	 */
	@Override
	public void handleKeyPressed(KeyEvent event) {
		if (event.keyCode == SWT.F5 && event.stateMask == 0) {
			if (refreshAction.isEnabled()) {
				refreshAction.refreshAll();
			}

			// Swallow the event
			event.doit = false;
		}
	}

	/**
	 * Returns whether there are builders configured on the given project.
	 * 
	 * @param project
	 *            the project
	 * @return <code>true</code> if it has builders, <code>false</code> if
	 *         not, or if this could not be determined
	 */
	boolean hasBuilder(IProject project) {
		try {
			final ICommand[] commands = project.getDescription().getBuildSpec();
			if (commands.length > 0) {
				return true;
			}
		} catch (final CoreException e) {
			// Cannot determine if project has builders. Project is closed
			// or does not exist. Fall through to return false.
		}
		return false;
	}

	@Override
	protected void makeActions() {
		final Shell shell = navigator.getSite().getShell();
		openProjectAction = new OpenResourceAction(shell);
		closeProjectAction = new CloseResourceAction(shell);
		refreshAction = new RefreshAction(shell);
		refreshAction.setDisabledImageDescriptor(ErlideUIPlugin.getDefault()
				.getImageDescriptor(ErlideUIConstants.IMG_DISABLED_REFRESH));
		refreshAction.setImageDescriptor(ErlideUIPlugin.getDefault()
				.getImageDescriptor(ErlideUIConstants.IMG_REFRESH));
		buildAction = new BuildAction(shell,
				IncrementalProjectBuilder.INCREMENTAL_BUILD);
		rebuildAction = new BuildAction(shell,
				IncrementalProjectBuilder.FULL_BUILD);
	}

	/**
	 * update the action bars
	 * 
	 * @see org.eclipse.ui.actions.ActionGroup#updateActionBars()
	 */
	@Override
	public void updateActionBars() {
		final IStructuredSelection selection = (IStructuredSelection) getContext()
				.getSelection();
		refreshAction.selectionChanged(selection);
		buildAction.selectionChanged(selection);
		rebuildAction.selectionChanged(selection);
		openProjectAction.selectionChanged(selection);
		closeProjectAction.selectionChanged(selection);
	}
}
