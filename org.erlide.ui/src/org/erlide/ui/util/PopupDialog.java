/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.util;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolTip;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

public class PopupDialog {

    private PopupDialog() {
    }

    static ToolTip win;

    public static void show(final String title, final String message,
            final int icon, final boolean centered) {
        final IWorkbench workbench = PlatformUI.getWorkbench();
        final IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
        final Display display = window == null ? workbench.getDisplay()
                : window.getShell().getDisplay();
        final Shell activeShell = display.getActiveShell();

        display.syncExec(new Runnable() {
            @Override
            public void run() {
                final int style = icon | (centered ? SWT.NONE : SWT.BALLOON);
                win = new ToolTip(activeShell, style);
                win.setMessage(message);
                win.setText(title);
                win.setAutoHide(true);

                if (centered) {
                    final Rectangle r = display.getPrimaryMonitor().getBounds();
                    final Rectangle rr = new Rectangle(0, 0, 200, 100);
                    win.setLocation(r.x + r.width / 2 - rr.width / 2, r.y
                            + r.height / 2 - rr.height / 2);
                } else {
                    final Rectangle shellBounds = activeShell.getBounds();
                    win.setLocation(shellBounds.x + shellBounds.width - 10,
                            shellBounds.y + shellBounds.height - 10);
                }
                win.setVisible(true);
            }

        });
    }

    public static void showBalloon(final String title, final String message,
            final int icon) {
        show(title, message, icon, false);
    }

    public static void showDialog(final String title, final String message,
            final int icon) {
        show(title, message, icon, true);
    }

}
