/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.util;

import org.eclipse.core.resources.IContainer;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ElementTreeSelectionDialog;
import org.eclipse.ui.dialogs.NewFolderDialog;
import org.eclipse.ui.views.navigator.ResourceComparator;
import org.erlide.ui.editors.erl.IErlangHelpContextIds;

/**
  */
public class FolderSelectionDialog extends ElementTreeSelectionDialog implements
        ISelectionChangedListener {

    private Button fNewFolderButton;
    private IContainer fSelectedContainer;

    public FolderSelectionDialog(final Shell parent,
            final ILabelProvider labelProvider,
            final ITreeContentProvider contentProvider) {
        super(parent, labelProvider, contentProvider);
        setComparator(new ResourceComparator(ResourceComparator.NAME));
    }

    @Override
    protected Control createDialogArea(final Composite parent) {
        final Composite result = (Composite) super.createDialogArea(parent);

        getTreeViewer().addSelectionChangedListener(this);

        final Button button = new Button(result, SWT.PUSH);
        button.setText("Create &New Folder...");
        button.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent event) {
                newFolderButtonPressed();
            }
        });
        button.setFont(parent.getFont());
        fNewFolderButton = button;

        applyDialogFont(result);

        PlatformUI
                .getWorkbench()
                .getHelpSystem()
                .setHelp(
                        parent,
                        IErlangHelpContextIds.BP_SELECT_DEFAULT_OUTPUT_FOLDER_DIALOG);

        return result;
    }

    private void updateNewFolderButtonState() {
        final IStructuredSelection selection = (IStructuredSelection) getTreeViewer()
                .getSelection();
        fSelectedContainer = null;
        if (selection.size() == 1) {
            final Object first = selection.getFirstElement();
            if (first instanceof IContainer) {
                fSelectedContainer = (IContainer) first;
            }
        }
        fNewFolderButton.setEnabled(fSelectedContainer != null);
    }

    protected void newFolderButtonPressed() {
        final NewFolderDialog dialog = new NewFolderDialog(getShell(),
                fSelectedContainer);
        if (dialog.open() == Window.OK) {
            final TreeViewer treeViewer = getTreeViewer();
            treeViewer.refresh(fSelectedContainer);
            final Object createdFolder = dialog.getResult()[0];
            treeViewer.reveal(createdFolder);
            treeViewer.setSelection(new StructuredSelection(createdFolder));
        }
    }

    @Override
    public void selectionChanged(final SelectionChangedEvent event) {
        updateNewFolderButtonState();
    }

}
