/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Vlad Dumitrescu
 *******************************************************************************/

package org.erlide.ui.views.outline;

import java.util.List;

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.actions.ActionContext;
import org.eclipse.ui.actions.ActionGroup;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.eclipse.ui.model.WorkbenchAdapter;
import org.eclipse.ui.part.IPageSite;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModelChangeListener;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.ISourceReference;
import org.erlide.runtime.ErlLogger;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.actions.CompositeActionGroup;
import org.erlide.ui.actions.ErlangSearchActionGroup;
import org.erlide.ui.actions.SortAction;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.erl.ISortableContentOutlinePage;
import org.erlide.ui.navigator.ErlElementSorter;
import org.erlide.ui.prefs.plugin.ErlEditorMessages;
import org.erlide.ui.util.ErlModelUtils;
import org.erlide.ui.util.ProblemsLabelDecorator;

/**
 * 
 * @author Vlad Dumitrescu
 * 
 */

public class ErlangOutlinePage extends ContentOutlinePage implements
		IErlModelChangeListener, ISortableContentOutlinePage {

	IErlModule myMdl;

	private ErlangEditor fEditor;
	private final String fToolTipText = "Sort";

	private CompositeActionGroup fActionGroups;

	/**
	 * 
	 * @param documentProvider
	 * 
	 * @param editor
	 * 
	 */
	public ErlangOutlinePage(final IDocumentProvider documentProvider,
			final ErlangEditor editor) {
		// myDocProvider = documentProvider;
		fEditor = editor;
		ErlangCore.getModel().addModelChangeListener(this);
	}

	/**
	 * 
	 * @param editorInput
	 * 
	 */
	public void setInput(final IEditorInput editorInput) {
		// ErlLogger.log("> outline set input "+editorInput);
		myMdl = ErlModelUtils.getModule(editorInput);
		// if (myMdl != null) {
		// try {
		// myMdl.open(null);
		// } catch (final ErlModelException e) {
		// e.printStackTrace();
		// }
		//
		// refresh();
		// }
	}

	public void refresh() {
		if (getTreeViewer() != null) {
			final Control c = getTreeViewer().getControl();
			if (c.isDisposed()) {
				return;
			}
			final Display d = c.getDisplay();
			d.asyncExec(new Runnable() {

				@SuppressWarnings("synthetic-access")
				public void run() {
					if (getTreeViewer().getControl() != null
							&& !getTreeViewer().getControl().isDisposed()) {
						// ErlLogger.log("*>> refreshing.");
						getTreeViewer().setInput(myMdl);
					}
				}
			});
		}
	}

	@Override
	public void createControl(final Composite parent) {
		super.createControl(parent);
		final TreeViewer viewer = getTreeViewer();
		viewer.setContentProvider(new ErlangContentProvider(true));
		final ErlangLabelProvider erlangLabelProvider = new ErlangLabelProvider();
		erlangLabelProvider.addLabelDecorator(new ProblemsLabelDecorator());
		viewer.setLabelProvider(erlangLabelProvider);
		viewer.addSelectionChangedListener(this);
		getTreeViewer().setAutoExpandLevel(0);
		getTreeViewer().setUseHashlookup(true);
		viewer.setInput(myMdl);

		final MenuManager manager = new MenuManager();
		manager.setRemoveAllWhenShown(true);
		manager.addMenuListener(new IMenuListener() {
			public void menuAboutToShow(final IMenuManager m) {
				// recursive loop?
				// menuAboutToShow(m);
				contextMenuAboutToShow(m);
			}
		});
		final IPageSite site = getSite();

		site.registerContextMenu(ErlangPlugin.PLUGIN_ID + ".outline", manager,
				viewer);
		fActionGroups = new CompositeActionGroup(
				new ActionGroup[] { new ErlangSearchActionGroup(this) });
		// register global actions
		final IActionBars actionBars = site.getActionBars();
		actionBars.setGlobalActionHandler(ITextEditorActionConstants.UNDO,
				fEditor.getAction(ITextEditorActionConstants.UNDO));
		actionBars.setGlobalActionHandler(ITextEditorActionConstants.REDO,
				fEditor.getAction(ITextEditorActionConstants.REDO));
		fActionGroups.fillActionBars(actionBars);
		registerToolbarActions(actionBars);
	}

	protected void contextMenuAboutToShow(final IMenuManager menu) {
		ErlideUIPlugin.createStandardGroups(menu);
		final IStructuredSelection selection = (IStructuredSelection) getSelection();
		fActionGroups.setContext(new ActionContext(selection));
		fActionGroups.fillContextMenu(menu);
	}

	static class NoModuleElement extends WorkbenchAdapter implements IAdaptable {

		/*
		 * 
		 * @see java.lang.Object#toString()
		 * 
		 */
		@Override
		public String toString() {
			return ErlEditorMessages.ErlangOutlinePage_error_NoTopLevelType;
		}

		/*
		 * 
		 * @see org.eclipse.core.runtime.IAdaptable#getAdapter(Class)
		 * 
		 */
		@SuppressWarnings("unchecked")
		public Object getAdapter(final Class clas) {
			if (clas == IWorkbenchAdapter.class) {
				return this;
			}
			return null;
		}
	}

	public void select(final ISourceReference reference) {
		if (getTreeViewer() != null) {
			ISelection s = getTreeViewer().getSelection();
			if (s instanceof IStructuredSelection) {
				final IStructuredSelection ss = (IStructuredSelection) s;
				final List<?> elements = ss.toList();
				if (!elements.contains(reference)) {
					s = reference == null ? StructuredSelection.EMPTY
							: new StructuredSelection(reference);
					getTreeViewer().setSelection(s, true);
				}
			}
		}
	}

	@Override
	public void dispose() {
		if (fEditor == null) {
			return;
		}

		fEditor.outlinePageClosed();
		fEditor = null;

		ErlangCore.getModel().removeModelChangeListener(this);

		super.dispose();
	}

	public void elementChanged(final IErlElement element) {
		if (myMdl == element) {
			refresh();
		}
	}

	/**
	 * @param actionBars
	 */
	private void registerToolbarActions(final IActionBars actionBars) {
		final IToolBarManager toolBarManager = actionBars.getToolBarManager();
		final ViewerComparator vc = new ErlElementSorter();
		toolBarManager.add(new SortAction(getTreeViewer(), fToolTipText, vc,
				null, false, ErlideUIPlugin.getDefault().getPreferenceStore()));
	}

	public void sort(final boolean sorting) {
		ErlLogger.debug("sorting " + sorting);
	}
}