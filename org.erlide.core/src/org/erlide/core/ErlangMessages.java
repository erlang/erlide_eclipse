/*******************************************************************************
 * Copyright (c) 2000, 2003 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.core;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

public class ErlangMessages {

    private static final String RESOURCE_BUNDLE = "org.erlide.core.ErlangMessages"; //$NON-NLS-1$

    private static final ResourceBundle fgResourceBundle = ResourceBundle
            .getBundle(RESOURCE_BUNDLE);

    public static String getString(final String key) {
        try {
            return fgResourceBundle.getString(key);
        } catch (final MissingResourceException e) {
            return "!" + key + "!"; //$NON-NLS-2$ //$NON-NLS-1$
        }
    }
}
