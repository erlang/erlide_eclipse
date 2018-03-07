/*******************************************************************************
 * Copyright (c) 2000, 2011 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.erlide.ui.editors.erl.actions;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.action.Action;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.erl.ErlangEditorMessages;

public class GotoMatchingBracketAction extends Action {

    public final static String GOTO_MATCHING_BRACKET = "GotoMatchingBracket"; //$NON-NLS-1$

    private final ErlangEditor fEditor;

    public GotoMatchingBracketAction(final ErlangEditor editor) {
        super(ErlangEditorMessages.GotoMatchingBracket_label);
        Assert.isNotNull(editor);
        fEditor = editor;
        setEnabled(true);
        // PlatformUI.getWorkbench().getHelpSystem()
        // .setHelp(this, IJavaHelpContextIds.GOTO_MATCHING_BRACKET_ACTION);
    }

    @Override
    public void run() {
        fEditor.gotoMatchingBracket();
    }
}
