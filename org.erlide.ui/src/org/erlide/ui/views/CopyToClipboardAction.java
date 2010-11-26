/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.views;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.swt.SWTError;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.IAbstractTextEditorHelpContextIds;
import org.erlide.ui.actions.SelectionDispatchAction;

class CopyToClipboardAction extends SelectionDispatchAction {

    private static final int MAX_REPEAT_COUNT = 10;

    private Clipboard fClipboard;

    public CopyToClipboardAction(final IWorkbenchSite site) {
        super(site);

        setText("Copy");
        setToolTipText("Copy To Clipboard");
        setDescription("Copies the selected text to the clipboard");

        final ISharedImages workbenchImages = PlatformUI.getWorkbench()
                .getSharedImages();
        setDisabledImageDescriptor(workbenchImages
                .getImageDescriptor(ISharedImages.IMG_TOOL_COPY_DISABLED));
        setImageDescriptor(workbenchImages
                .getImageDescriptor(ISharedImages.IMG_TOOL_COPY));
        setHoverImageDescriptor(workbenchImages
                .getImageDescriptor(ISharedImages.IMG_TOOL_COPY));

        PlatformUI.getWorkbench().getHelpSystem()
                .setHelp(this, IAbstractTextEditorHelpContextIds.COPY_ACTION);

        update(getSelection());
    }

    @Override
    public void selectionChanged(final ITextSelection selection) {
        setEnabled(selection != null && selection.getLength() > 0);
    }

    @Override
    public void run(final ITextSelection selection) {
        fClipboard = new Clipboard(getShell().getDisplay());
        try {
            copyToClipboard(selection, 0);
        } finally {
            fClipboard.dispose();
        }
    }

    private void copyToClipboard(final ITextSelection selection,
            final int repeatCount) {
        try {
            fClipboard.setContents(new String[] { selection.getText() },
                    new Transfer[] { TextTransfer.getInstance() });
        } catch (final SWTError e) {
            if (e.code != DND.ERROR_CANNOT_SET_CLIPBOARD
                    || repeatCount >= MAX_REPEAT_COUNT) {
                throw e;
            }

            if (MessageDialog.openQuestion(getShell(), "Error",
                    "Copy to clipboard failed")) {
                copyToClipboard(selection, repeatCount + 1);
            }
        }
    }
}
