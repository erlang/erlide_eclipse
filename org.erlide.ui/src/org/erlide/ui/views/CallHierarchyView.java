/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.views;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.util.OpenStrategy;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.jinterface.backend.Backend;
import org.erlide.ui.editors.util.EditorUtility;

import erlang.ErlangXref;
import erlang.FunctionRef;

public class CallHierarchyView extends ViewPart {
	private Tree tree;
	private TreeViewer treeViewer;
	private Label lblRoot;

	private static class ViewerLabelProvider extends LabelProvider {
		@Override
		public Image getImage(Object element) {
			return super.getImage(element);
		}

		@Override
		public String getText(Object element) {
			if (element instanceof IErlFunction) {
				IErlFunction ref = (IErlFunction) element;
				String n = ref.getModule().getModuleName();
				return n + " : " + ref.toString();
			}
			return super.getText(element);
		}
	}

	private class TreeContentProvider implements ITreeContentProvider {

		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			if (newInput instanceof IErlFunction) {
				IErlFunction fun = (IErlFunction) newInput;
				lblRoot.setText(fun.getModule().getModuleName() + " : "
						+ fun.getNameWithArity());
			}
		}

		public void dispose() {
		}

		public Object[] getElements(Object inputElement) {
			return getChildren(inputElement);
		}

		public Object[] getChildren(Object parentElement) {
			IErlFunction parent = (IErlFunction) parentElement;
			FunctionRef ref = new FunctionRef(parent);
			Backend b = ErlangCore.getBackendManager().getIdeBackend();
			FunctionRef[] children = ErlangXref.functionUse(b, ref);
			if (children == null) {
				return new IErlFunction[0];
			}
			List<IErlFunction> result = new ArrayList<IErlFunction>();
			for (FunctionRef r : children) {
				IErlFunction fun = parent.getModel().findFunction(r);
				if (fun != null) {
					result.add(fun);
				}
			}

			return result.toArray(new IErlFunction[result.size()]);
		}

		public Object getParent(Object element) {
			return null;
		}

		public boolean hasChildren(Object element) {
			return getChildren(element).length > 0;
		}
	}

	public CallHierarchyView() {
		Backend b = ErlangCore.getBackendManager().getIdeBackend();
		ErlangXref.start(b);
	}

	@Override
	public void createPartControl(Composite parent) {
		{
			final Composite composite = new Composite(parent, SWT.NONE);
			composite.setLayout(new GridLayout(2, false));
			{
				lblRoot = new Label(composite, SWT.NONE);
				lblRoot.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
						false, 1, 1));
				lblRoot.setText("<no function>");
			}
			{
				final ToolBar toolBar = new ToolBar(composite, SWT.FLAT);
				toolBar.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, true,
						false, 1, 1));
				{
					final ToolItem tltmRefresh = new ToolItem(toolBar, SWT.NONE);
					tltmRefresh.addSelectionListener(new SelectionAdapter() {
						@Override
						public void widgetSelected(SelectionEvent e) {
							Backend b = ErlangCore.getBackendManager()
									.getIdeBackend();
							ErlangXref.update(b);
							treeViewer.refresh();
						}
					});
					tltmRefresh.setText("refresh");
				}
			}
			{
				treeViewer = new TreeViewer(composite, SWT.BORDER);
				treeViewer.setLabelProvider(new ViewerLabelProvider());
				treeViewer.setContentProvider(new TreeContentProvider());
				tree = treeViewer.getTree();
				tree.addMouseListener(new MouseAdapter() {
					@Override
					public void mouseDoubleClick(MouseEvent e) {
						TreeItem[] sel = tree.getSelection();
						IErlFunction el = (IErlFunction) sel[0].getData();
						final boolean activateOnOpen = getSite() != null ? true
								: OpenStrategy.activateOnOpen();
						try {
							// TODO we want to find the exact place in the code
							EditorUtility.openElementInEditor(el,
									activateOnOpen);
						} catch (PartInitException e1) {
							e1.printStackTrace();
						} catch (ErlModelException e1) {
							e1.printStackTrace();
						}
					}
				});
				tree.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false,
						true, 2, 1));
			}
		}
	}

	@Override
	public void dispose() {
		Backend b = ErlangCore.getBackendManager().getIdeBackend();
		ErlangXref.stop(b);
		super.dispose();
	}

	@Override
	public void setFocus() {
		tree.setFocus();
	}

	public void setRoot(IErlFunction ref) {
		if (ref == null) {
			return;
		}
		treeViewer.setInput(ref);
		// treeViewer.expandToLevel(2);
		treeViewer.refresh();
	}

}
