/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.views.outline;

import java.util.List;

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.eclipse.ui.model.WorkbenchAdapter;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlElementDelta;
import org.erlide.core.erlang.IErlModelChangeListener;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IParent;
import org.erlide.core.erlang.ISourceReference;
import org.erlide.core.erlang.util.ElementChangedEvent;
import org.erlide.core.erlang.util.IElementChangedListener;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.prefs.plugin.ErlEditorMessages;
import org.erlide.ui.util.ErlModelUtils;

/**
 * 
 * 
 * @author Vlad Dumitrescu
 */
public class ErlangOutlinePage extends ContentOutlinePage implements
		IErlModelChangeListener {

	IErlModule myMdl;

	private ErlangEditor myEditor;

	// private IDocumentProvider myDocProvider;

	/**
	 * @param documentProvider
	 * @param editor
	 */
	public ErlangOutlinePage(IDocumentProvider documentProvider,
			ErlangEditor editor) {
		// myDocProvider = documentProvider;
		myEditor = editor;
		ErlangCore.getModel().addModelChangeListener(this);
	}

	/**
	 * @param editorInput
	 */
	public void setInput(IEditorInput editorInput) {
		// System.out.println("> outline set input "+editorInput);
		myMdl = ErlModelUtils.getModule(editorInput);
		if (myMdl != null) {
			try {
				myMdl.open(null);
			} catch (final ErlModelException e) {
				e.printStackTrace();
			}

			refresh();
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
						// System.out.println("*>> refreshing.");
						getTreeViewer().setInput(myMdl);
					}
				}
			});
		}
	}

	@Override
	public void createControl(Composite parent) {
		super.createControl(parent);
		final TreeViewer viewer = getTreeViewer();
		viewer.setContentProvider(new ChildrenProvider());
		viewer.setLabelProvider(new ErlangLabelProvider());
		viewer.addSelectionChangedListener(this);
		getTreeViewer().setAutoExpandLevel(TreeViewer.ALL_LEVELS);
		viewer.setInput(myMdl);
	}

	class ChildrenProvider implements ITreeContentProvider {

		/* FIXME: NO_MODULE -- remove or use */
		@SuppressWarnings("unused")
		private final Object[] NO_MODULE = new Object[] { new NoModuleElement() };

		private final Object[] NO_CHILDREN = new Object[] {};

		private ElementChangedListener fListener;

		public Object[] getChildren(Object parent) {
			if (parent instanceof IParent) {
				final IParent c = (IParent) parent;
				try {
					return c.getChildren();
				} catch (final ErlModelException x) {
					if (!x.isDoesNotExist()) {
						System.out
								.println("element missing: " + x.getMessage());
					}
				}
			}
			return NO_CHILDREN;
		}

		public Object[] getElements(Object parent) {
			if (parent instanceof IParent) {
				try {
					return ((IParent) parent).getChildren();
				} catch (final ErlModelException e) {
					e.printStackTrace();
					return NO_CHILDREN;
				}
			}
			return getChildren(parent);
		}

		public Object getParent(Object child) {
			if (child instanceof IErlElement) {
				final IErlElement e = (IErlElement) child;
				return e.getParent();
			}
			return null;
		}

		public boolean hasChildren(Object parent) {
			if (parent instanceof IParent) {
				final IParent c = (IParent) parent;
				try {
					final IErlElement[] children = c.getChildren();
					return (children != null && children.length > 0);
				} catch (final ErlModelException x) {
					if (!x.isDoesNotExist()) {
						System.out
								.println("element missing: " + x.getMessage());
					}
				}
			}
			return false;
		}

		public boolean isDeleted(Object o) {
			return false;
		}

		public void dispose() {
			if (fListener != null) {
				ErlangCore.getModelManager().removeElementChangedListener(
						fListener);
				fListener = null;
			}
		}

		/*
		 * @see IContentProvider#inputChanged(Viewer, Object, Object)
		 */
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			if (oldInput == newInput) {
				return;
			}
			boolean isModule = (newInput instanceof IErlModule);

			// System.out.println("> cprov set input:::" + newInput);
			if (newInput != null) {
				// System.out.println("!! " + newInput.getClass().getName() + "
				// " +
				// fListener);
			}
			if (isModule && fListener == null) {
				fListener = new ElementChangedListener();
				ErlangCore.getModelManager().addElementChangedListener(
						fListener);
			} else if (!isModule && fListener != null) {
				ErlangCore.getModelManager().removeElementChangedListener(
						fListener);
				fListener = null;
			}
		}
	}

	static class NoModuleElement extends WorkbenchAdapter implements IAdaptable {

		/*
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			return ErlEditorMessages.ErlangOutlinePage_error_NoTopLevelType;
		}

		/*
		 * @see org.eclipse.core.runtime.IAdaptable#getAdapter(Class)
		 */
		public Object getAdapter(Class clas) {
			if (clas == IWorkbenchAdapter.class) {
				return this;
			}
			return null;
		}
	}

	class ElementChangedListener implements IElementChangedListener {

		public void elementChanged(final ElementChangedEvent e) {

			if (getControl() == null) {
				return;
			}

			final Display d = getControl().getDisplay();
			if (d != null) {
				d.asyncExec(new Runnable() {

					public void run() {
						// IErlModule cu = (IErlModule) fInput;
						// IErlElement base = cu;
						// base = getMainType(cu);
						// if (base == null)
						// {
						// if (fOutlineViewer != null)
						// fOutlineViewer.refresh(true);
						// return;
						// }
						// IErlElementDelta delta = findElement(base,
						// e.getDelta());
						// if (delta != null && fOutlineViewer != null)
						// {
						// fOutlineViewer.reconcile(delta);
						// }

					}
				});
			}
		}

		/* FIXME: isPossibleStructuralChange -- needed, or rudiment? */
		@SuppressWarnings("unused")
		private boolean isPossibleStructuralChange(IErlElementDelta cuDelta) {
			if (cuDelta.getKind() != IErlElementDelta.CHANGED) {
				return true; // add or remove
			}
			final int flags = cuDelta.getFlags();
			if ((flags & IErlElementDelta.F_CHILDREN) != 0) {
				return true;
			}
			return (flags & (IErlElementDelta.F_CONTENT | IErlElementDelta.F_FINE_GRAINED)) == IErlElementDelta.F_CONTENT;
		}

	}

	public void select(ISourceReference reference) {
		if (getTreeViewer() != null) {
			ISelection s = getTreeViewer().getSelection();
			if (s instanceof IStructuredSelection) {
				final IStructuredSelection ss = (IStructuredSelection) s;
				final List elements = ss.toList();
				if (!elements.contains(reference)) {
					s = (reference == null ? StructuredSelection.EMPTY
							: new StructuredSelection(reference));
					getTreeViewer().setSelection(s, true);
				}
			}
		}
	}

	@Override
	public void dispose() {
		if (myEditor == null) {
			return;
		}

		myEditor.outlinePageClosed();
		myEditor = null;

		ErlangCore.getModel().removeModelChangeListener(this);

		super.dispose();
	}

	public void elementChanged(IErlElement element) {
		if (myMdl == element) {
			refresh();
		}
	}
}
