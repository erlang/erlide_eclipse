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
package org.erlide.wrangler.refactoring.ui;

import java.util.HashMap;

import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Widget;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;
import org.erlide.wrangler.refactoring.util.IErlRange;
import org.erlide.wrangler.refactoring.util.WranglerUtils;

/**
 * For SelectionInputDialog every selectable element is connected with an object
 * from this class, to handle mouseover events
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class ExpressionCheckButtonListener implements MouseTrackListener {

    private final IErlMemberSelection selection;
    private final HashMap<Button, IErlRange> checkButtons;

    /**
     * Constructor
     * 
     * @param checkButtons
     *            Elements which should be monitored
     */
    public ExpressionCheckButtonListener(
            final HashMap<Button, IErlRange> checkButtons) {
        this.checkButtons = checkButtons;
        selection = (IErlMemberSelection) GlobalParameters
                .getWranglerSelection();
    }

    @Override
    public void mouseEnter(final MouseEvent e) {
        setHighlight(e.widget);
    }

    @Override
    public void mouseExit(final MouseEvent e) {
        resetHighlight();
    }

    @Override
    public void mouseHover(final MouseEvent e) {
    }

    private void setHighlight(final Widget w) {
        final int offset = checkButtons.get(w).getOffset();
        final int length = checkButtons.get(w).getLength();
        WranglerUtils.highlightSelection(offset, length, selection);

    }

    private void resetHighlight() {
        WranglerUtils.highlightSelection(selection.getSelectionRange()
                .getOffset(), selection.getSelectionRange().getLength(),
                selection);
    }
}
