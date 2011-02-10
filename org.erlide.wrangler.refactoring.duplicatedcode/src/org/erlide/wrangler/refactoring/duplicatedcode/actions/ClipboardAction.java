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
package org.erlide.wrangler.refactoring.duplicatedcode.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.widgets.Display;

/**
 * Handles the 'copy to clipboard' action.
 * 
 * @author Gyorgy Orosz
 * 
 */
public class ClipboardAction extends Action {
    private final Display display;
    private String text;

    /**
     * Constructor
     * 
     * @param display
     *            actual SWT display
     */
    public ClipboardAction(final Display display) {
        this.display = display;
    }

    @Override
    public void run() {
        final Clipboard cb = new Clipboard(display);
        final TextTransfer textTransfer = TextTransfer.getInstance();
        cb.setContents(new Object[] { text },
                new TextTransfer[] { textTransfer });
    }

    @Override
    public void setText(final String text) {
        this.text = text;
    }
}
