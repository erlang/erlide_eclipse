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

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.IBaseLabelProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProviderChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Tree;
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
import org.erlide.core.erlang.ErlModelException;
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
import org.erlide.ui.util.ProblemsLabelDecorator.ProblemsLabelChangedEvent;

/**
 * 
 * @author Vlad Dumitrescu
 * 
 */

public class ErlangOutlinePage extends ContentOutlinePage implements
		IErlModelChangeListener, ISortableContentOutlinePage {

	public class ErlangOutlineViewer extends TreeViewer {

		public ErlangOutlineViewer(final Tree tree) {
			super(tree);
			setAutoExpandLevel(0);
			setUseHashlookup(true);
		}

		/*
		 * @see ContentViewer#handleLabelProviderChanged(LabelProviderChangedEvent)
		 */
		@Override
		protected void handleLabelProviderChanged(
				LabelProviderChangedEvent event) {
			final Object input = getInput();
			if (event instanceof ProblemsLabelChangedEvent) {
				final ProblemsLabelChangedEvent e = (ProblemsLabelChangedEvent) event;
				if (e.isMarkerChange() && input instanceof IErlModule) {
					return; // marker changes can be ignored
				}
			}
			// look if the underlying resource changed
			final Object[] changed = event.getElements();
			if (changed != null) {
				final IResource resource = getUnderlyingResource();
				if (resource != null) {
					for (int i = 0; i < changed.length; i++) {
						if (changed[i] != null && changed[i].equals(resource)) {
							// change event to a full refresh
							event = new LabelProviderChangedEvent(
									(IBaseLabelProvider) event.getSource());
							break;
						}
					}
				}
			}
			super.handleLabelProviderChanged(event);
		}

		private IResource getUnderlyingResource() {
			if (fModule != null) {
				return fModule.getResource();
			}
			return null;
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.views.contentoutline.ContentOutlinePage#getControl()
	 */
	@Override
	public Control getControl() {
		return fOutlineViewer.getControl();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.views.contentoutline.ContentOutlinePage#getSelection()
	 */
	@Override
	public ISelection getSelection() {
		return fOutlineViewer.getSelection();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.views.contentoutline.ContentOutlinePage#getTreeViewer()
	 */
	@Override
	protected TreeViewer getTreeViewer() {
		return fOutlineViewer;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.views.contentoutline.ContentOutlinePage#setFocus()
	 */
	@Override
	public void setFocus() {
		getControl().setFocus();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.views.contentoutline.ContentOutlinePage#setSelection(org
	 *      .eclipse.jface.viewers.ISelection)
	 */
	@Override
	public void setSelection(final ISelection selection) {
		fOutlineViewer.setSelection(selection);
	}

	private IErlModule fModule;

	private ErlangEditor fEditor;

	private CompositeActionGroup fActionGroups;

	private ErlangOutlineViewer fOutlineViewer;

	private MemberFilterActionGroup fMemberFilterActionGroup;

	private final IDocumentProvider fDocumentProvider;

	private SortAction fSortAction;

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
		fDocumentProvider = documentProvider;
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
		fModule = ErlModelUtils.getModule(editorInput, fDocumentProvider);
		try {
			if (fModule != null) {
				fModule.open(null);
			}
		} catch (final ErlModelException e) {
		}
	}

	public void refresh() {
		if (getTreeViewer() != null) {
			final Control c = getTreeViewer().getControl();
			if (c.isDisposed()) {
				return;
			}
			final Display d = c.getDisplay();
			d.asyncExec(new Runnable() {

				public void run() {
					if (getTreeViewer().getControl() != null
							&& !getTreeViewer().getControl().isDisposed()) {
						// ErlLogger.log("*>> refreshing.");
						getTreeViewer().setInput(fModule);
					}
				}
			});
		}
	}

	@Override
	public void createControl(final Composite parent) {
		// FIXME we still don't get a popup menu, we should have one...

		final Tree tree = new Tree(parent, SWT.MULTI);
		fOutlineViewer = new ErlangOutlineViewer(tree);
		fOutlineViewer.setContentProvider(fEditor
				.createOutlineContentProvider());
		fOutlineViewer.setLabelProvider(fEditor.createOutlineLabelProvider());
		fOutlineViewer.addPostSelectionChangedListener(this);
		fOutlineViewer.setInput(fModule);

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
		final Menu menu = manager.createContextMenu(tree);
		tree.setMenu(menu);

		site.registerContextMenu(ErlangPlugin.PLUGIN_ID + ".outline", manager,
				fOutlineViewer);
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

	public static class NoModuleElement extends WorkbenchAdapter implements
			IAdaptable {

		/*
		 * 
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			return ErlEditorMessages.ErlangOutlinePage_error_noelement;
		}

		/*
		 * 
		 * @see org.eclipse.core.runtime.IAdaptable#getAdapter(Class)
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
		if (fEditor != null) {
			fEditor.outlinePageClosed();
			fEditor = null;
		}
		if (fMemberFilterActionGroup != null) {
			fMemberFilterActionGroup.dispose();
			fMemberFilterActionGroup = null;
		}
		ErlangCore.getModel().removeModelChangeListener(this);

		super.dispose();
	}

	public void elementChanged(final IErlElement element) {
		if (fModule == element) {
			refresh();
		}
	}

	/**
	 * @param actionBars
	 */
	private void registerToolbarActions(final IActionBars actionBars) {
		final IToolBarManager toolBarManager = actionBars.getToolBarManager();
		fSortAction = new SortAction(getTreeViewer(), "Sort",
				new ErlElementSorter(ErlElementSorter.SORT_ON_NAME),
				new ErlElementSorter(ErlElementSorter.SORT_ON_EXPORT), null,
				false, ErlideUIPlugin.getDefault().getPreferenceStore());
		toolBarManager.add(fSortAction);
		fMemberFilterActionGroup = new MemberFilterActionGroup(fOutlineViewer,
				"org.eclipse.jdt.ui.JavaOutlinePage"); //$NON-NLS-1$
		fMemberFilterActionGroup.contributeToToolBar(toolBarManager);
	}

	public void sort(final boolean sorting) {
		ErlLogger.debug("sorting " + sorting);
	}
}