/*******************************************************************************
 * Copyright (c) 2000, 2010 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.internal.information;

import java.net.URL;

import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.erlide.util.ErlLogger;

public class OpenBrowserUtil {

    /**
     * Opens the given url in the browser as choosen in the preferences.
     *
     * @param url
     *            the URL
     * @param display
     *            the display
     * @since 3.6
     */
    public static void open(final URL url, final Display display) {
        display.syncExec(new Runnable() {
            @Override
            public void run() {
                internalOpen(url, false);
            }
        });
    }

    /**
     * Opens the given URL in an external browser.
     *
     * @param url
     *            the URL
     * @param display
     *            the display
     * @since 3.6
     */
    public static void openExternal(final URL url, final Display display) {
        display.syncExec(new Runnable() {
            @Override
            public void run() {
                internalOpen(url, true);
            }
        });
    }

    private static void internalOpen(final URL url, final boolean useExternalBrowser) {
        BusyIndicator.showWhile(null, new Runnable() {
            @Override
            public void run() {
                final URL helpSystemUrl = PlatformUI.getWorkbench().getHelpSystem()
                        .resolve(url.toExternalForm(), true);
                try {
                    final IWorkbenchBrowserSupport browserSupport = PlatformUI
                            .getWorkbench().getBrowserSupport();
                    IWebBrowser browser;
                    if (useExternalBrowser) {
                        browser = browserSupport.getExternalBrowser();
                    } else {
                        browser = browserSupport.createBrowser(null);
                    }
                    browser.openURL(helpSystemUrl);
                } catch (final PartInitException ex) {
                    // XXX: show dialog?
                    ErlLogger.error(ex);
                }
            }
        });
    }

}
