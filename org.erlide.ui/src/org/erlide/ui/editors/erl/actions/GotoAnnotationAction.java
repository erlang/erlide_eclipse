/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.editors.erl.actions;

import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.erl.ErlangEditorMessages;
import org.erlide.ui.editors.erl.IErlangHelpContextIds;

public class GotoAnnotationAction extends TextEditorAction {

    private final boolean fForward;

    public GotoAnnotationAction(final String prefix, final boolean forward) {
        super(ErlangEditorMessages.getBundleForConstructedKeys(), prefix, null);

        fForward = forward;
        if (forward) {
            PlatformUI
                    .getWorkbench()
                    .getHelpSystem()
                    .setHelp(this, IErlangHelpContextIds.GOTO_NEXT_ERROR_ACTION);
        } else {
            PlatformUI
                    .getWorkbench()
                    .getHelpSystem()
                    .setHelp(this,
                            IErlangHelpContextIds.GOTO_PREVIOUS_ERROR_ACTION);
        }
    }

    @Override
    public void run() {
        final ErlangEditor e = (ErlangEditor) getTextEditor();
        e.gotoAnnotation(fForward);
    }

    @Override
    public void setEditor(final ITextEditor editor) {
        if (editor instanceof ErlangEditor) {
            super.setEditor(editor);
        }
        update();
    }

    @Override
    public void update() {
        setEnabled(getTextEditor() instanceof ErlangEditor);
    }
}
