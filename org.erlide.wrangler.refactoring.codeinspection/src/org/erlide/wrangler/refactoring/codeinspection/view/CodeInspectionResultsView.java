/*******************************************************************************
 * Copyright (c) 2010 György Orosz.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     György Orosz - initial API and implementation
 ******************************************************************************/
package org.erlide.wrangler.refactoring.codeinspection.view;

import java.util.ArrayList;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;
import org.erlide.core.model.erlang.IErlFunctionClause;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.IErlElement;
import org.erlide.wrangler.refactoring.util.WranglerUtils;

/**
 * View which handles textual data referencing to an Erlang element, got from
 * code inspection functions
 * 
 * @author Gyorgy Orosz
 * 
 */
public class CodeInspectionResultsView extends ViewPart {
    public static final String VIEW_ID = "org.erlide.wrangler.refactoring.codeinspection.codeinspectionresultview";

    ArrayList<IErlElement> results = new ArrayList<IErlElement>();

    private TableViewer viewer;

    class ViewContentProvider implements IStructuredContentProvider {
        @Override
        public void inputChanged(final Viewer v, final Object oldInput,
                final Object newInput) {
        }

        @Override
        public void dispose() {
        }

        @Override
        public Object[] getElements(final Object parent) {
            return results.toArray();
        }
    }

    class ViewLabelProvider extends LabelProvider implements
            ITableLabelProvider {
        @Override
        public String getColumnText(final Object obj, final int index) {
            final IErlElement e = (IErlElement) obj;
            if (e instanceof IErlModule) {
                final IErlModule m = (IErlModule) e;
                return m.getModuleName() + "\t\t- "
                        + e.getResource().getFullPath().toString();
            } else if (e instanceof IErlFunctionClause) {
                final IErlFunctionClause fc = (IErlFunctionClause) e;
                return fc.getModule().getModuleName() + ":" + fc.toString();
            }
            return e.toString();
        }

        @Override
        public Image getColumnImage(final Object obj, final int index) {
            return getImage(obj);
        }

        @Override
        public Image getImage(final Object obj) {
            if (obj instanceof IErlModule) {
                return PlatformUI.getWorkbench().getSharedImages()
                        .getImage(ISharedImages.IMG_OBJ_FILE);
            } else if (obj instanceof IErlFunctionClause) {
                return PlatformUI.getWorkbench().getSharedImages()
                        .getImage(ISharedImages.IMG_OBJ_ELEMENT);
            } else {
                return null;
            }
        }
    }

    @Override
    public void createPartControl(final Composite parent) {
        viewer = new TableViewer(parent, SWT.MULTI | SWT.H_SCROLL
                | SWT.V_SCROLL);
        viewer.setContentProvider(new ViewContentProvider());
        viewer.setLabelProvider(new ViewLabelProvider());
        viewer.setInput(getViewSite());
        viewer.addDoubleClickListener(new IDoubleClickListener() {

            @Override
            public void doubleClick(final DoubleClickEvent event) {

                final IStructuredSelection s = (IStructuredSelection) event
                        .getSelection();
                final Object o = s.getFirstElement();
                if (o instanceof IErlModule) {
                    final IErlModule m = (IErlModule) o;
                    WranglerUtils.openFile((IFile) m.getResource());
                } else if (o instanceof IErlFunctionClause) {
                    WranglerUtils.highlightSelection((IErlFunctionClause) o);
                }

            }

        });

    }

    /**
     * Passes the focus request to the viewer's control.
     */
    @Override
    public void setFocus() {
        viewer.getControl().setFocus();
    }

    /**
     * Refresh the view, to show the latest content.
     */
    public void refresh() {
        try {
            viewer.refresh();
        } catch (final Exception e) {
            e.printStackTrace();
        }

    }

    /**
     * Adds Erlang elements to the view
     * 
     * @param e
     *            Erlang elements' list
     */
    public void addElements(final ArrayList<IErlElement> e) {
        results = e;
    }

    /**
     * Sets the view title
     * 
     * @param title
     *            title
     */
    public void setViewTitle(final String title) {
        setPartName(title);

    }
}
