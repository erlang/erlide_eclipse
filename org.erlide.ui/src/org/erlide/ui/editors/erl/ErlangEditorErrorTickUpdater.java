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
package org.erlide.ui.editors.erl;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.Assert;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Shell;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.util.IProblemChangedListener;

/**
 * The <code>ErlangEditorErrorTickUpdater</code> will register as a
 * IProblemChangedListener to listen on problem changes of the editor's input.
 * It updates the title images when the annotation model changed.
 */
public class ErlangEditorErrorTickUpdater implements IProblemChangedListener {

    final ErlangEditor fErlangEditor;
    private final ErlangFileLabelProvider fLabelProvider;

    public ErlangEditorErrorTickUpdater(final ErlangEditor editor) {
        Assert.isNotNull(editor);
        fErlangEditor = editor;
        fLabelProvider = new ErlangFileLabelProvider();
        ErlideUIPlugin.getDefault().getProblemMarkerManager().addListener(this);
    }

    @Override
    public void problemsChanged(final IResource[] changedResources,
            final boolean isMarkerChange) {
        if (isMarkerChange) {
            final IErlModule module = fErlangEditor.getModule();
            if (module != null) {
                final IResource resource = module.getResource();
                for (int i = 0; i < changedResources.length; i++) {
                    if (changedResources[i].equals(resource)) {
                        updateEditorImage(module);
                    }
                }
            }
        }
    }

    public void updateEditorImage(final IErlModule module) {
        final Image titleImage = fErlangEditor.getTitleImage();
        if (titleImage == null) {
            return;
        }
        Image newImage;
        // if (jelement instanceof ICompilationUnit
        // && !jelement.getErlProject().isOnClasspath(jelement)) {
        // newImage = fLabelProvider.getImage(jelement.getResource());
        // } else {
        newImage = fLabelProvider.getImage(module);
        // }
        if (titleImage != newImage) {
            postImageChange(newImage);
        }
    }

    private void postImageChange(final Image newImage) {
        final Shell shell = fErlangEditor.getEditorSite().getShell();
        if (shell != null && !shell.isDisposed()) {
            shell.getDisplay().syncExec(new Runnable() {
                @Override
                public void run() {
                    fErlangEditor.updatedTitleImage(newImage);
                }
            });
        }
    }

    public void dispose() {
        fLabelProvider.dispose();
        ErlideUIPlugin.getDefault().getProblemMarkerManager()
                .removeListener(this);
    }

}
