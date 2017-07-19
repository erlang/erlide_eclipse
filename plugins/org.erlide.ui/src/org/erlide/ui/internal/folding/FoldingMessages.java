package org.erlide.ui.internal.folding;

import org.eclipse.osgi.util.NLS;

/**
 * Helper class to get NLSed messages.
 */
final class FoldingMessages extends NLS {

    private static final String BUNDLE_NAME = FoldingMessages.class.getName();

    private FoldingMessages() {
        // Do not instantiate
    }

    public static String DefaultErlangFoldingPreferenceBlock_title;

    public static String DefaultErlangFoldingPreferenceBlock_comments;

    public static String DefaultErlangFoldingPreferenceBlock_functions;

    public static String DefaultErlangFoldingPreferenceBlock_clauses;

    public static String DefaultErlangFoldingPreferenceBlock_edoc;

    public static String DefaultErlangFoldingPreferenceBlock_header_comments;

    public static String DefaultErlangFoldingPreferenceBlock_macro_declarations;

    public static String DefaultErlangFoldingPreferenceBlock_exports;

    public static String DefaultErlangFoldingPreferenceBlock_typespecs;

    public static String EmptyErlangFoldingPreferenceBlock_emptyCaption;

    static {
        NLS.initializeMessages(BUNDLE_NAME, FoldingMessages.class);
    }
}
