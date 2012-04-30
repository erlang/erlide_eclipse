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
import org.erlide.backend.BackendCore;
import org.erlide.backend.IBackend;
import org.erlide.core.model.erlang.FunctionRef;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.services.search.ErlangXref;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.editors.util.EditorUtility;

public class CallHierarchyView extends ViewPart {
    Tree tree;
    TreeViewer treeViewer;
    Label lblRoot;

    static class ViewerLabelProvider extends LabelProvider {
        @Override
        public Image getImage(final Object element) {
            return super.getImage(element);
        }

        @Override
        public String getText(final Object element) {
            if (element instanceof IErlFunction) {
                final IErlFunction ref = (IErlFunction) element;
                final String n = ref.getModuleName();
                return n + " : " + ref.toString();
            }
            return super.getText(element);
        }
    }

    class TreeContentProvider implements ITreeContentProvider {

        private Object input;

        @Override
        public void inputChanged(final Viewer viewer, final Object oldInput,
                final Object newInput) {
            input = newInput;
            if (newInput instanceof IErlFunction) {
                final IErlFunction fun = (IErlFunction) newInput;
                lblRoot.setText(fun.getModuleName() + " : "
                        + fun.getNameWithArity());
            } else if (input instanceof String) {
                lblRoot.setText((String) input);
            }
        }

        @Override
        public void dispose() {
        }

        @Override
        public Object[] getElements(final Object inputElement) {
            return getChildren(inputElement);
        }

        @Override
        public Object[] getChildren(final Object parentElement) {
            if (parentElement instanceof String) {
                return new Object[0];
            }
            final IErlFunction parent = (IErlFunction) parentElement;
            final FunctionRef ref = new FunctionRef(parent);
            final IBackend b = BackendCore.getBackendManager().getIdeBackend();
            final FunctionRef[] children = ErlangXref.functionUse(b, ref);
            if (children == null) {
                return new Object[0];
            }
            if (parentElement == input && children.length == 0) {
                // TODO ErlangXref should cache _all_ projects added to it
                return new Object[] { "<no callers from project "
                        + parent.getModule().getProject().getName() + ">" };
            }
            final List<IErlFunction> result = new ArrayList<IErlFunction>();
            for (final FunctionRef r : children) {
                try {
                    final IErlFunction fun = parent.getModel().findFunction(r);
                    if (fun != null) {
                        result.add(fun);
                    }
                } catch (final ErlModelException e) {
                    ErlLogger.error(e);
                }
            }
            return result.toArray(new IErlFunction[result.size()]);
        }

        @Override
        public Object getParent(final Object element) {
            return null;
        }

        @Override
        public boolean hasChildren(final Object element) {
            return getChildren(element).length > 0;
        }
    }

    public CallHierarchyView() {
        final IBackend b = BackendCore.getBackendManager().getIdeBackend();
        ErlangXref.start(b);
    }

    @Override
    public void createPartControl(final Composite parent) {
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
                        public void widgetSelected(final SelectionEvent e) {
                            final IBackend b = BackendCore.getBackendManager()
                                    .getIdeBackend();
                            ErlangXref.update(b);
                            treeViewer.refresh();
                        }
                    });
                    tltmRefresh.setText("refresh");
                }
            }
            {
                treeViewer = new TreeViewer(composite, SWT.NONE);
                treeViewer.setLabelProvider(new ViewerLabelProvider());
                treeViewer.setContentProvider(new TreeContentProvider());
                tree = treeViewer.getTree();
                tree.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mouseDoubleClick(final MouseEvent e) {
                        final TreeItem[] sel = tree.getSelection();
                        final IErlFunction el = (IErlFunction) sel[0].getData();
                        final boolean activateOnOpen = getSite() != null ? true
                                : OpenStrategy.activateOnOpen();
                        try {
                            // TODO we want to find the exact place in the code
                            EditorUtility.openElementInEditor(el,
                                    activateOnOpen);
                        } catch (final PartInitException e1) {
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
        final IBackend b = BackendCore.getBackendManager().getIdeBackend();
        ErlangXref.stop(b);
        super.dispose();
    }

    @Override
    public void setFocus() {
        tree.setFocus();
    }

    public void setRoot(final IErlFunction ref) {
        if (ref == null) {
            return;
        }
        treeViewer.setInput(ref);
        // treeViewer.expandToLevel(2);
        treeViewer.refresh();
    }

    public void setMessage(final String msg) {
        if (msg == null) {
            return;
        }
        treeViewer.setInput(msg);
        treeViewer.refresh();
    }

}
