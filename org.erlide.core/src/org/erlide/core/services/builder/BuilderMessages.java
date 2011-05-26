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
package org.erlide.core.services.builder;

import org.eclipse.osgi.util.NLS;

public final class BuilderMessages extends NLS {

    private static final String BUNDLE_NAME = "org.erlide.core.services.builder.messages"; //$NON-NLS-1$

    private BuilderMessages() {
    }

    public static String build_oneWarning;
    public static String build_multipleWarnings;
    public static String build_multipleErrors;
    public static String build_oneError;
    public static String build_compiling;
    public static String build_done;
    public static String build_foundHeader;
    public static String build_fixedHeader;
    public static String build_inconsistentProject;
    public static String build_missingSourceFile;
    public static String build_preparingBuild;
    public static String build_readingDelta;
    public static String build_dialyzerProblem;

    static {
        NLS.initializeMessages(BUNDLE_NAME, BuilderMessages.class);
    }

}
