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
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
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
import org.erlide.core.erlang.IErlModelChangeListener;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.ISourceReference;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.erl.ISortableContentOutlinePage;
import org.erlide.ui.prefs.plugin.ErlEditorMessages;
import org.erlide.ui.util.ErlModelUtils;

/**
 * 
 * 
 * @author Vlad Dumitrescu
 */
public class ErlangOutlinePage extends ContentOutlinePage implements
		IErlModelChangeListener, ISortableContentOutlinePage {

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
		// ErlLogger.log("> outline set input "+editorInput);
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
					if (getTreeViewer().getControl() != null &&
							!getTreeViewer().getControl().isDisposed()) {
						// ErlLogger.log("*>> refreshing.");
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
		viewer.setContentProvider(new ErlangContentProvider(true));
		viewer.setLabelProvider(new ErlangLabelProvider());
		viewer.addSelectionChangedListener(this);
		getTreeViewer().setAutoExpandLevel(0);
		getTreeViewer().setUseHashlookup(true);
		viewer.setInput(myMdl);
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
		@SuppressWarnings("unchecked")
		public Object getAdapter(Class clas) {
			if (clas == IWorkbenchAdapter.class) {
				return this;
			}
			return null;
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

	public void sort(boolean sorting) {
	}

}
