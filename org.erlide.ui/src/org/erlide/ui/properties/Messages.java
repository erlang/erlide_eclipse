package org.erlide.ui.properties;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
    private static final String BUNDLE_NAME = "org.erlide.ui.properties.messages"; //$NON-NLS-1$
    public static String OldErlProjectPropertyPage_uz_text;
    public static String OldErlProjectPropertyPage_outLabel_text;
    public static String OldErlProjectPropertyPage_uz_toolTipText;
    public static String OldErlProjectPropertyPage_l1_text;
    public static String OldErlProjectPropertyPage_source_toolTipText;
    public static String OldErlProjectPropertyPage_includesLabel_text;
    public static String OldErlProjectPropertyPage_include_toolTipText;
    public static String OldErlProjectPropertyPage_nodeNameLabel_1_text;
    public static String OldErlProjectPropertyPage_runtimeVersion_toolTipText;
    public static String OldErlProjectPropertyPage_btnAddPath_text;
    public static String OldErlProjectPropertyPage_btnRemove_text;
    public static String OldErlProjectPropertyPage_testSources_toolTipText;

    // //////////////////////////////////////////////////////////////////////////
    //
    // Constructor
    //
    // //////////////////////////////////////////////////////////////////////////
    private Messages() {
        // do not instantiate
    }

    // //////////////////////////////////////////////////////////////////////////
    //
    // Class initialization
    //
    // //////////////////////////////////////////////////////////////////////////
    static {
        // load message values from bundle file
        NLS.initializeMessages(BUNDLE_NAME, Messages.class);
    }
}
