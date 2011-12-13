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
package org.erlide.wrangler.refactoring.duplicatedcode.ui;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;
import org.erlide.wrangler.refactoring.duplicatedcode.actions.ClipboardAction;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.AbstractResultTreeObject;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeInstanceElement;

/**
 * Duplicates view
 * 
 * @author Gyorgy Orosz
 * 
 */
public class DuplicatesView extends ViewPart {
    // private final class HighlightAction extends Action {
    // public void run() {
    // ISelection selection = viewer.getSelection();
    // Object obj = ((IStructuredSelection) selection)
    // .getFirstElement();
    // showMessage("Double-click detected on " + obj.toString());
    // }
    // }

    ISelectionListener listener = new ISelectionListener() {

        @Override
        public void selectionChanged(final IWorkbenchPart part,
                final ISelection selection) {
            MessageDialog.openInformation(getSite().getShell(), "test",
                    selection.toString());

        }
    };

    private TreeViewer viewer;
    private Action copyGeneralisedToClipboard;
    private final Action copyFunCallToClipboard = new ClipboardAction(
            PlatformUI.getWorkbench().getDisplay()) {
        private boolean hasText = false;

        @Override
        public void run() {
            if (!hasText) {
                MessageDialog
                        .openInformation(PlatformUI.getWorkbench()
                                .getActiveWorkbenchWindow().getShell(),
                                "Empty clipboard",
                                "There is no FunCall element according the selected item!");
            } else {
                super.run();
            }
        }

        @Override
        public void setText(final String text) {
            super.setText(text);
            hasText = true;
        }

    };

    /**
     * The constructor.
     */
    public DuplicatesView() {
    }

    /**
     * Refresh the view
     */
    public void refresh() {
        viewer.refresh();
    }

    /**
     * This is a callback that will allow us to create the viewer and initialize
     * it.
     */
    @Override
    public void createPartControl(final Composite parent) {
        viewer = new TreeViewer(parent, SWT.SINGLE | SWT.H_SCROLL
                | SWT.V_SCROLL);
        // drillDownAdapter = new DrillDownAdapter(viewer);
        viewer.setContentProvider(new DuplicatesViewContentProvider(this));
        viewer.setLabelProvider(new DuplicatesViewLabelProvider());
        // viewer.setSorter(new NameSorter());
        viewer.setInput(getViewSite());
        makeActions();
        createToolbar();
        hookDoubleClickAction();
        // contributeToActionBars();
        addListeners();

    }

    // private void hookContextMenu() {
    // MenuManager menuMgr = new MenuManager("#PopupMenu");
    // menuMgr.setRemoveAllWhenShown(true);
    // menuMgr.addMenuListener(new IMenuListener() {
    // public void menuAboutToShow(IMenuManager manager) {
    // DuplicatedCodeView.this.fillContextMenu(manager);
    // }
    // });
    // Menu menu = menuMgr.createContextMenu(viewer.getControl());
    // viewer.getControl().setMenu(menu);
    // getSite().registerContextMenu(menuMgr, viewer);
    // }
    //
    // private void contributeToActionBars() {
    // IActionBars bars = getViewSite().getActionBars();
    // fillLocalPullDown(bars.getMenuManager());
    // fillLocalToolBar(bars.getToolBarManager());
    // }

    // private void fillLocalPullDown(IMenuManager manager) {
    // manager.add(action1);
    // // manager.add(new Separator());
    // // manager.add(action2);
    // }
    //
    // private void fillContextMenu(IMenuManager manager) {
    // manager.add(action1);
    // // manager.add(action2);
    // manager.add(new Separator());
    // drillDownAdapter.addNavigationActions(manager);
    // // Other plug-ins can contribute there actions here
    // manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
    // }
    //
    // private void fillLocalToolBar(IToolBarManager manager) {
    // // manager.add(action1);
    // // manager.add(action2);
    // // manager.add(new Separator());
    // drillDownAdapter.addNavigationActions(manager);
    // }

    private void addListeners() {
        // getSite().getWorkbenchWindow().getSelectionService()
        // .addSelectionListener(listener);

    }

    private void createToolbar() {
        final IToolBarManager mgr = getViewSite().getActionBars()
                .getToolBarManager();
        mgr.add(copyGeneralisedToClipboard);
        mgr.add(copyFunCallToClipboard);

    }

    private void makeActions() {

        copyGeneralisedToClipboard = new ClipboardAction(PlatformUI
                .getWorkbench().getDisplay());
        copyGeneralisedToClipboard
                .setToolTipText("Copy generalised function to the clipboard");
        copyGeneralisedToClipboard.setImageDescriptor(PlatformUI.getWorkbench()
                .getSharedImages()
                .getImageDescriptor(ISharedImages.IMG_TOOL_COPY));
        copyFunCallToClipboard.setToolTipText("Copy FunCall to the clipboard");
        copyFunCallToClipboard.setImageDescriptor(PlatformUI.getWorkbench()
                .getSharedImages()
                .getImageDescriptor(ISharedImages.IMG_TOOL_COPY));

        viewer.addSelectionChangedListener(new ISelectionChangedListener() {
            @Override
            public void selectionChanged(final SelectionChangedEvent event) {
                final ISelection sel = event.getSelection();
                if (sel == null || sel.isEmpty()) {
                    return;
                }
                final TreeSelection tsel = (TreeSelection) sel;
                final AbstractResultTreeObject selection = (AbstractResultTreeObject) tsel
                        .getFirstElement();
                copyGeneralisedToClipboard.setText(selection.getSuggestedCode());

                if (selection instanceof DuplicatedCodeInstanceElement) {
                    final DuplicatedCodeInstanceElement dcie = (DuplicatedCodeInstanceElement) selection;
                    copyFunCallToClipboard.setText(dcie
                            .getReplicationFunction());
                }

            }
        });

    }

    private void hookDoubleClickAction() {
        viewer.addDoubleClickListener(new DoubleClickListener());
    }

    /**
     * Passing the focus request to the viewer's control.
     */
    @Override
    public void setFocus() {
        viewer.getControl().setFocus();
    }
}
