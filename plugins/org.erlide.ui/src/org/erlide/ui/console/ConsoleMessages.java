/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * IBM - Initial API and implementation
 *******************************************************************************/
package org.erlide.ui.console;

import org.eclipse.osgi.util.NLS;

public class ConsoleMessages extends NLS {
    private static final String BUNDLE_NAME = "org.erlide.ui.console.ConsoleMessages";//$NON-NLS-1$

    public static String ConsoleRemoveAllTerminatedAction_0;
    public static String ConsoleRemoveAllTerminatedAction_1;

    public static String ConsoleTerminateAction_0;
    public static String ConsoleTerminateAction_1;

    public static String ProcessConsole_0;

    public static String ProcessConsole_1;
    public static String ProcessConsole_2;

    static {
        // load message values from bundle file
        NLS.initializeMessages(BUNDLE_NAME, ConsoleMessages.class);
    }

    public static String ConsoleRemoveTerminatedAction_0;

    public static String ConsoleRemoveTerminatedAction_1;

    public static String ConsoleShowPreferencesAction__run_debug_console;

    public static String ShowStandardErrorAction_0;

    public static String ShowStandardOutAction_0;
}
