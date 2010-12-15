/*******************************************************************************
 * Copyright (c) 2003 Berthold Daum.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Berthold Daum
 *******************************************************************************/

package com.bdaum.overlayPages;

import java.util.ResourceBundle;

public class Messages {

    private final static String RESOURCE_BUNDLE = "com.bdaum.overlayPages.Messages";//$NON-NLS-1$

    private static ResourceBundle fgResourceBundle = null;

    private static boolean notRead = true;

    public Messages() {
    }

    public static ResourceBundle getResourceBundle() {
        if (notRead) {
            notRead = false;
            try {
                fgResourceBundle = ResourceBundle.getBundle(RESOURCE_BUNDLE);
            } catch (final Exception e) {
            }
        }

        return fgResourceBundle;
    }

    public static String getString(final String key) {
        try {
            return getResourceBundle().getString(key);
        } catch (final Exception e) {
            return "!" + key + "!";//$NON-NLS-2$ //$NON-NLS-1$
        }
    }
}
