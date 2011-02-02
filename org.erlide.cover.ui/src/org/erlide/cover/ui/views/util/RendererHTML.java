package org.erlide.cover.ui.views.util;

/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others. All rights reserved.
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors: IBM Corporation - initial API and implementation
 ******************************************************************************/
/*
 * Browser example snippet: render HTML that includes relative links from memory
 * 
 * For a list of all SWT example snippets see
 * http://dev.eclipse.org/viewcvs/index.cgi/%7Echeckout%7E/platform-swt-home/dev.html#snippets
 */

import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 * Displays html reports
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class RendererHTML {

    public static void main(final String[] args) {
        /* Relative links: use the HTML base tag */
        final String html = "<html><head>"
                + "<base href=\"http://dev.eclipse.org/viewcvs/index.cgi/%7Echeckout%7E/platform-swt-home/\" >"
                + "<title>HTML Test</title></head>"
                + "<body><a href=\"dev.html\">local link</a></body></html>";

        final Display display = new Display();
        final Shell shell = new Shell(display);
        shell.setLayout(new FillLayout());
        final Browser browser = new Browser(shell, SWT.NONE);
        browser.setText(html);
        shell.open();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }
        display.dispose();
    }
}
