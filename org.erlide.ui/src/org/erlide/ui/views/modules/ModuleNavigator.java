/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *******************************************************************************/
package org.erlide.ui.views.modules;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeViewerListener;
import org.eclipse.jface.viewers.TreeExpansionEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.actions.ActionContext;
import org.eclipse.ui.actions.OpenFileAction;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.views.navigator.ResourceSelectionUtil;
import org.erlide.basiccore.ErlLogger;
import org.erlide.core.erlang.IErlElement;
import org.erlide.ui.views.modules.actions.MainActionGroup;
import org.erlide.ui.views.modules.actions.ModuleNavigatorActionGroup;

/**
 * Provides Resource Navigator/Package Explorer type functionality for erlang
 * projects.
 * 
 * 
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class ModuleNavigator extends ViewPart {

	OpenFileAction openFileAction;

	Action doubleClickAction;

	/**
	 * The view id
	 */
	public static final String ID = "org.erlide.ui.views.modulenavigator";

	/**
	 * The jface tree view
	 */
	private TreeViewer viewer;

	/**
	 * The system action group
	 */
	private ModuleNavigatorActionGroup actionGroup;

	/**
	 * Creates the action group, which encapsulates all actions for the view.
	 */
	protected void makeActions() {
		setActionGroup(new MainActionGroup(this));
		openFileAction = new OpenFileAction(getSite().getPage());

		doubleClickAction = new Action() {

			@Override
			public void run() {
				final IStructuredSelection selection = (IStructuredSelection) getSelection();
				final boolean anyResourceSelected = !selection.isEmpty() &&
						ResourceSelectionUtil.allResourcesAreOfType(selection,
								IResource.PROJECT | IResource.FOLDER |
										IResource.FILE);
				final boolean onlyFilesSelected = anyResourceSelected &&
						ResourceSelectionUtil.allResourcesAreOfType(selection,
								IResource.FILE);

				if (onlyFilesSelected) {
					openFileAction.selectionChanged(selection);
					openFileAction.run();
				}
			}
		};
	}

	/**
	 * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createPartControl(Composite parent) {
		viewer = new TreeViewer(parent);
		viewer.setContentProvider(new ModuleContentProvider());
		viewer.setLabelProvider(new ModuleLabelProvider());
		viewer.setInput(ResourcesPlugin.getWorkspace().getRoot());
		viewer.addTreeListener(fExpansionListener);

		final IWorkbenchPartSite site = getSite();
		site.getPage().addPartListener(fPartListener);

		makeActions();
		hookDoubleClickAction();

		// Fill the action bars and update the global action handlers'
		// enabled state to match the current selection.
		getActionGroup().fillActionBars(getViewSite().getActionBars());
		updateActionBars((IStructuredSelection) viewer.getSelection());

		createContextMenu();
	}

	/**
	 * Updates the action bar actions.
	 * 
	 * @param selection
	 *            the current selection
	 */
	protected void updateActionBars(IStructuredSelection selection) {
		final ModuleNavigatorActionGroup group = getActionGroup();
		if (group != null) {
			group.setContext(new ActionContext(selection));
			group.updateActionBars();
		}
	}

	/**
	 * Create the context menu system
	 * 
	 */
	private void createContextMenu() {
		// Create the manager
		final MenuManager menuMgr = new MenuManager();
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {

			public void menuAboutToShow(IMenuManager mgr) {
				fillContextMenu(mgr);
			}
		});

		// create menu
		final Menu menu = menuMgr.createContextMenu(viewer.getControl());
		viewer.getControl().setMenu(menu);

		// Register for extension
		getSite().registerContextMenu(menuMgr, viewer);
	}

	/**
	 * Add the menu items
	 * 
	 * @param mgr
	 */
	protected void fillContextMenu(IMenuManager mgr) {
		final IStructuredSelection selection = (IStructuredSelection) viewer
				.getSelection();
		getActionGroup().setContext(new ActionContext(selection));
		getActionGroup().fillContextMenu(mgr);
	}

	/**
	 * @return Returns the actionGroup.
	 */
	public ModuleNavigatorActionGroup getActionGroup() {
		return actionGroup;
	}

	/**
	 * @param lactionGroup
	 *            The actionGroup to set.
	 */
	public void setActionGroup(ModuleNavigatorActionGroup lactionGroup) {
		actionGroup = lactionGroup;
	}

	/**
	 * @return Returns the viewer.
	 */
	public TreeViewer getViewer() {
		return viewer;
	}

	private final ITreeViewerListener fExpansionListener = new ITreeViewerListener() {

		public void treeCollapsed(TreeExpansionEvent event) {
		}

		public void treeExpanded(TreeExpansionEvent event) {
			Object element = event.getElement();
			ErlLogger.log("ZZ exp::" + element.getClass().getName());
			if (element instanceof IErlElement) {
				expandMainType(element);
			}

		}
	};

	private final IPartListener fPartListener = new IPartListener() {

		public void partActivated(IWorkbenchPart part) {
			if (part instanceof IEditorPart) {
				editorActivated((IEditorPart) part);
			}
		}

		public void partBroughtToTop(IWorkbenchPart part) {
		}

		public void partClosed(IWorkbenchPart part) {
		}

		public void partDeactivated(IWorkbenchPart part) {
		}

		public void partOpened(IWorkbenchPart part) {
		}
	};

	/**
	 * @param part
	 */
	protected void editorActivated(IEditorPart part) {
		// TODO Auto-generated method stub
		ErlLogger.log("act::" + part.getClass().getName());
	}

	/**
	 * @param element
	 */
	protected void expandMainType(Object element) {
		// TODO Auto-generated method stub
		ErlLogger.log("exp::" + element.getClass().getName());
	}

	/**
	 * @see IWorkbenchPart#setFocus()
	 */
	@Override
	public void setFocus() {
		viewer.getTree().setFocus();
	}

	/**
	 * Returns the current selection.
	 */
	ISelection getSelection() {
		return viewer.getSelection();
	}

	/**
	 * Run the action associated with double click (Open the file)
	 * 
	 */
	private void hookDoubleClickAction() {
		viewer.addDoubleClickListener(new IDoubleClickListener() {

			public void doubleClick(DoubleClickEvent event) {
				doubleClickAction.run();
			}
		});
	}

}
