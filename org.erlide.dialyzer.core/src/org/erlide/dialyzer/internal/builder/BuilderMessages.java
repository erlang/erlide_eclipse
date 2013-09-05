package org.erlide.dialyzer.internal.builder;

import org.eclipse.osgi.util.NLS;

public final class BuilderMessages extends NLS {

    private static final String BUNDLE_NAME = "org.erlide.core.internal.builder.messages"; //$NON-NLS-1$

    private BuilderMessages() {
    }

    public static String build_dialyzerProblem;

    static {
        NLS.initializeMessages(BUNDLE_NAME, BuilderMessages.class);
    }

}
