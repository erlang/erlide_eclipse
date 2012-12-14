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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTError;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.LocationEvent;
import org.eclipse.swt.browser.LocationListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.program.Program;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.UIJob;
import org.erlide.ui.internal.ErlideUIPlugin;

import com.novocode.naf.swt.custom.BalloonWindow;

public class PopupDialog {

    private PopupDialog() {
    }

    static BalloonWindow win;

    public static void show(final String title, final String message,
            final int delay, final int anchor, final boolean centered) {
        final IWorkbench workbench = PlatformUI.getWorkbench();
        final IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
        final Display display = window == null ? workbench.getDisplay()
                : window.getShell().getDisplay();

        // balloon
        display.syncExec(new Runnable() {
            @Override
            public void run() {
                win = new BalloonWindow(display, SWT.ON_TOP | SWT.TITLE
                        | SWT.TOOL);
                win.setText(title);

                final Composite ct = win.getContents();

                final GridLayout layout = new GridLayout(1, false);
                ct.setLayout(layout);

                final GridData data1 = new GridData(SWT.FILL, SWT.FILL, true,
                        true);
                data1.widthHint = 400;
                data1.heightHint = 200;
                try {
                    final Browser browser = new Browser(ct, SWT.NONE);
                    browser.setLayoutData(data1);
                    browser.setBackground(ct.getBackground());
                    browser.setForeground(ct.getForeground());
                    browser.setText(buildMessageDocument(message,
                            browser.getFont(), browser.getBackground()));
                    browser.addLocationListener(new LocationListener() {
                        @Override
                        public void changing(final LocationEvent event) {
                            Program.launch(event.location);
                            event.doit = false;
                            win.close();
                        }

                        @Override
                        public void changed(final LocationEvent event) {
                        }
                    });
                } catch (final SWTError e) {
                    // failed to create browser, use a label instead
                    final Label l = new Label(ct, SWT.WRAP);
                    l.setLayoutData(data1);
                    l.setBackground(ct.getBackground());
                    l.setForeground(ct.getForeground());
                    l.setText(cleanupMarkup(message));
                    win.addSelectionControl(l);
                }

                final Label l2 = new Label(ct, SWT.WRAP);
                l2.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
                l2.setBackground(ct.getBackground());
                l2.setForeground(ct.getForeground());
                l2.setText("(close)");

                ct.pack();

                final Rectangle r = display.getPrimaryMonitor().getBounds();
                final Rectangle rr = win.getContents().getBounds();
                if (centered) {
                    win.setLocation(r.x + r.width / 2 - rr.width / 2, r.y
                            + r.height / 2 - rr.height / 2);
                } else {
                    final Shell activeShell = display.getActiveShell();
                    if (activeShell != null) {
                        final Rectangle shellBounds = activeShell.getBounds();
                        win.setLocation(shellBounds.x + shellBounds.width - 10,
                                shellBounds.y + shellBounds.height - 10);
                    } else {
                        win.setLocation(r.x + r.width - 10, r.y + r.height - 30);
                    }
                }
                win.setAnchor(anchor);
                win.setVisible(true);
            }

        });
        if (delay >= 0) {
            final UIJob job = new UIJob("close balloon") {
                @Override
                public IStatus runInUIThread(final IProgressMonitor monitor) {
                    if (!win.getContents().isDisposed()) {
                        win.getContents().getShell().close();
                        // display.dispose();
                    }
                    return new Status(IStatus.OK, ErlideUIPlugin.PLUGIN_ID,
                            IStatus.OK, "", null);
                }
            };
            job.schedule(delay);
        }
    }

    public static void showBalloon(final String title, final String message,
            final int delay) {
        show(title, message, delay, SWT.RIGHT | SWT.BOTTOM, false);
    }

    public static void showDialog(final String title, final String message,
            final int delay) {
        show(title, message, delay, SWT.NONE, true);
    }

    public static void showModalDialog(final String title, final String message) {
        showModalDialog(title, message, -1);
    }

    public static void showModalDialog(final String title,
            final String message, final int delay) {
        show(title, message, delay, SWT.NONE, true);
    }

    private static String buildMessageDocument(final String message,
            final Font font, final Color background) {
        final FontData[] data = font.getFontData();
        if (data == null || data.length == 0) {
            return message;
        }

        final StringBuilder builder = new StringBuilder();
        builder.append("<html><body>");
        builder.append("<head>");
        builder.append("<style type=\"text/css\"> \n");
        builder.append("body\n");
        builder.append("{\n");
        builder.append("background-color:#");
        final int rgb = background.getRed() << 16 | background.getGreen() << 8
                | background.getBlue();
        builder.append(Integer.toHexString(rgb));
        builder.append(";\n");
        builder.append("font-family:\"");
        builder.append(data[0].getName());
        builder.append("\";\n");
        builder.append("font-size:");
        // builder.append(data[0].getHeight());
        builder.append(14);
        builder.append("px;\n");
        builder.append("}\n");
        builder.append("</style>\n");
        builder.append("</head>\n");

        builder.append(message);

        builder.append("</html></body>");

        return builder.toString();
    }

    private static String cleanupMarkup(final String message) {
        return message
                .replaceAll("<a href=\"([^\"]+)\">([^<]+)</a>", "$2 [$1]")
                .replaceAll("<[^>]+>", "");
    }
}
