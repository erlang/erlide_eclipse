/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Eric Merritt
 *******************************************************************************/
package org.erlide.ui.views.modules.actions;

import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.SameShellProvider;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.actions.ActionContext;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.actions.ExportResourcesAction;
import org.eclipse.ui.actions.ImportResourcesAction;
import org.eclipse.ui.actions.NewWizardMenu;
import org.eclipse.ui.dialogs.PropertyDialogAction;
import org.erlide.basicui.util.PluginUtils;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.IErlideUIConstants;
import org.erlide.ui.views.modules.ModuleNavigator;

/**
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class MainActionGroup extends ModuleNavigatorActionGroup {

	protected PropertyDialogAction propertyDialogAction;

	protected ImportResourcesAction importAction;

	protected ExportResourcesAction exportAction;

	protected CollapseAllAction collapseAllAction;

	protected GotoActionGroup gotoGroup;

	protected OpenActionGroup openGroup;

	protected WorkspaceActionGroup workspaceGroup;

	private final IResourceChangeListener resourceChangeListener;

	/**
	 * Constructs the main action group.
	 * 
	 * @param lnavigator
	 *            the module navigator
	 */
	public MainActionGroup(ModuleNavigator lnavigator) {
		super(lnavigator);
		resourceChangeListener = new IResourceChangeListener() {

			public void resourceChanged(IResourceChangeEvent event) {
				handleResourceChanged(event);
			}
		};
		ResourcesPlugin.getWorkspace().addResourceChangeListener(
				resourceChangeListener, IResourceChangeEvent.POST_CHANGE);
		makeSubGroups();
	}

	/**
	 * Handles a resource changed event by updating the enablement if one of the
	 * selected projects is opened or closed.
	 * 
	 * @param event
	 *            the change event
	 */
	protected void handleResourceChanged(IResourceChangeEvent event) {
		final ActionContext context = getContext();
		if (context == null) {
			return;
		}

		final IStructuredSelection selection = (IStructuredSelection) context
				.getSelection();
		if (!(PluginUtils.allResourcesAreOfType(selection, IResource.PROJECT))) {
			return;
		}
		final List<?> sel = selection.toList();
		final IResourceDelta delta = event.getDelta();
		if (delta == null) {
			return;
		}
		final IResourceDelta[] projDeltas = delta
				.getAffectedChildren(IResourceDelta.CHANGED);
		for (final IResourceDelta projDelta : projDeltas) {
			if ((projDelta.getFlags() & IResourceDelta.OPEN) != 0) {
				if (sel.contains(projDelta.getResource())) {
					getNavigator().getSite().getShell().getDisplay().syncExec(
							new Runnable() {

								public void run() {
									gotoGroup.updateActionBars();
									workspaceGroup.updateActionBars();
								}
							});
				}
			}
		}
	}

	/**
	 * Makes the actions contained directly in this action group.
	 */
	@Override
	protected void makeActions() {
		final Shell shell = navigator.getSite().getShell();

		propertyDialogAction = new PropertyDialogAction(new SameShellProvider(
				shell), navigator.getViewer());

		importAction = new ImportResourcesAction(navigator.getSite()
				.getWorkbenchWindow());
		importAction.setDisabledImageDescriptor(ErlideUIPlugin.getDefault()
				.getImageDescriptor(IErlideUIConstants.IMG_DISABLED_IMPORT));
		importAction.setImageDescriptor(ErlideUIPlugin.getDefault()
				.getImageDescriptor(IErlideUIConstants.IMG_IMPORT));

		exportAction = new ExportResourcesAction(navigator.getSite()
				.getWorkbenchWindow());
		exportAction.setDisabledImageDescriptor(ErlideUIPlugin.getDefault()
				.getImageDescriptor(IErlideUIConstants.IMG_DISABLED_EXPORT));
		exportAction.setImageDescriptor(ErlideUIPlugin.getDefault()
				.getImageDescriptor(IErlideUIConstants.IMG_EXPORT));

		collapseAllAction = new CollapseAllAction(
				navigator,
				ErlideUIPlugin
						.getResourceString("views.modules.actions.main.collapsealltitle"));
		collapseAllAction
				.setToolTipText(ErlideUIPlugin
						.getResourceString("views.modules.actions.main.collapsealltooltip"));
		collapseAllAction.setImageDescriptor(ErlideUIPlugin.getDefault()
				.getImageDescriptor(IErlideUIConstants.IMG_COLLAPSEALL));

	}

	/**
	 * Makes the sub action groups.
	 */
	protected void makeSubGroups() {
		gotoGroup = new GotoActionGroup(navigator);
		openGroup = new OpenActionGroup(navigator);
		workspaceGroup = new WorkspaceActionGroup(navigator);
	}

	/**
	 * Extends the superclass implementation to set the context in the
	 * subgroups.
	 * 
	 * @param context
	 */
	@Override
	public void setContext(ActionContext context) {
		super.setContext(context);
		gotoGroup.setContext(context);
		openGroup.setContext(context);
		workspaceGroup.setContext(context);
	}

	/**
	 * Fills the context menu with the actions contained in this group and its
	 * subgroups.
	 * 
	 * @param menu
	 *            the context menu
	 */
	@Override
	public void fillContextMenu(IMenuManager menu) {
		final IStructuredSelection selection = (IStructuredSelection) getContext()
				.getSelection();

		new NewWizardMenu(navigator.getSite().getWorkbenchWindow());

		gotoGroup.fillContextMenu(menu);
		openGroup.fillContextMenu(menu);
		menu.add(new Separator());

		menu.add(importAction);
		menu.add(exportAction);
		importAction.selectionChanged(selection);
		exportAction.selectionChanged(selection);
		menu.add(new Separator());

		workspaceGroup.fillContextMenu(menu);

		menu.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
		menu
				.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS
						+ "-end")); //$NON-NLS-1$
		menu.add(new Separator());

		if (selection.size() == 1) {
			propertyDialogAction.selectionChanged(selection);
			menu.add(propertyDialogAction);
		}
	}

	/**
	 * Adds the actions in this group and its subgroups to the action bars.
	 * 
	 * @param actionBars
	 */
	@Override
	public void fillActionBars(IActionBars actionBars) {
		actionBars.setGlobalActionHandler(ActionFactory.PROPERTIES.getId(),
				propertyDialogAction);

		gotoGroup.fillActionBars(actionBars);
		openGroup.fillActionBars(actionBars);
		workspaceGroup.fillActionBars(actionBars);

		final IToolBarManager toolBar = actionBars.getToolBarManager();
		toolBar.add(new Separator());
		toolBar.add(collapseAllAction);
	}

	/**
	 * Updates the actions which were added to the action bars, delegating to
	 * the subgroups as necessary.
	 */
	@Override
	public void updateActionBars() {
		final IStructuredSelection selection = (IStructuredSelection) getContext()
				.getSelection();
		propertyDialogAction.setEnabled(selection.size() == 1);
		gotoGroup.updateActionBars();
		openGroup.updateActionBars();
		workspaceGroup.updateActionBars();
	}

	/**
	 * Runs the default action (open file) by delegating the open group.
	 * 
	 * @param selection
	 *            the selection
	 */
	@Override
	public void runDefaultAction(IStructuredSelection selection) {
		openGroup.runDefaultAction(selection);
	}

	/**
	 * Handles a key pressed event by invoking the appropriate action,
	 * delegating to the subgroups as necessary.
	 * 
	 * @param event
	 */
	@Override
	public void handleKeyPressed(KeyEvent event) {
		workspaceGroup.handleKeyPressed(event);
	}

	/**
	 * Extends the superclass implementation to dispose the actions in this
	 * group and its subgroups.
	 */
	@Override
	public void dispose() {
		ResourcesPlugin.getWorkspace().removeResourceChangeListener(
				resourceChangeListener);

		collapseAllAction.dispose();
		exportAction.dispose();
		importAction.dispose();
		propertyDialogAction.dispose();

		gotoGroup.dispose();
		openGroup.dispose();
		workspaceGroup.dispose();
		super.dispose();
	}

}
