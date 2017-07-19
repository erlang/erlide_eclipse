/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui;

import org.eclipse.osgi.util.NLS;

public class ErlideUIMessages extends NLS {

    private static final String BUNDLE_NAME = "org.erlide.ui.ErlideUIMessages";//$NON-NLS-1$

    public static String QuickOutlinePopupDialog_infoTextPressEscToExit;
    public static String PDEMultiPageContentOutline_SortingAction_tooltip;

    public static String ExceptionHandler_seeErrorLogMessage;

    static {
        // load message values from bundle file
        NLS.initializeMessages(BUNDLE_NAME, ErlideUIMessages.class);
    }

}
