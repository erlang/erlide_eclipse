/*******************************************************************************
 * Copyright (c) 2004 IBM and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.graphics.Image;
import org.erlide.ui.internal.ErlideUIPlugin;

/**
 * Bundle of most images used by the Erlang debug plug-in.
 */
public class ErlideUIDebugImages {

    /* Declare Common paths */
    private static URL ICON_BASE_URL = null;

    static {
        final String pathSuffix = "icons/full/"; //$NON-NLS-1$
        ICON_BASE_URL = ErlideUIPlugin.getDefault().getBundle()
                .getEntry(pathSuffix);
    }

    // The plugin registry
    private static ImageRegistry fgImageRegistry = null;

    /*
     * Available cached Images in the Erlang debug plug-in image registry.
     */
    public static final String IMG_OBJS_EXCEPTION = "IMG_OBJS_EXCEPTION"; //$NON-NLS-1$

    public static final String IMG_OBJS_EXCEPTION_DISABLED = "IMG_OBJS_EXCEPTION_DISABLED"; //$NON-NLS-1$

    public static final String IMG_OBJS_ERROR = "IMG_OBJS_ERROR"; //$NON-NLS-1$	

    public static final String IMG_OVR_BREAKPOINT_INSTALLED = "IMG_OBJS_BREAKPOINT_INSTALLED"; //$NON-NLS-1$

    public static final String IMG_OVR_BREAKPOINT_INSTALLED_DISABLED = "IMG_OBJS_BREAKPOINT_INSTALLED_DISABLED"; //$NON-NLS-1$

    public static final String IMG_OBJS_LOCAL_VARIABLE = "IMG_OBJS_LOCAL_VARIABLE"; //$NON-NLS-1$

    public static final String IMG_OVR_METHOD_BREAKPOINT_ENTRY = "IMG_OBJS_METHOD_BREAKPOINT_ENTRY"; //$NON-NLS-1$

    public static final String IMG_OVR_METHOD_BREAKPOINT_ENTRY_DISABLED = "IMG_OBJS_METHOD_BREAKPOINT_ENTRY_DISABLED"; //$NON-NLS-1$

    public static final String IMG_OVR_METHOD_BREAKPOINT_EXIT = "IMG_OBJS_METHOD_BREAKPOINT_EXIT"; //$NON-NLS-1$

    public static final String IMG_OVR_METHOD_BREAKPOINT_EXIT_DISABLED = "IMG_OBJS_METHOD_BREAKPOINT_EXIT_DISABLED"; //$NON-NLS-1$

    public static final String IMG_OVR_CONDITIONAL_BREAKPOINT = "IMG_OBJS_CONDITIONAL_BREAKPOINT"; //$NON-NLS-1$

    public static final String IMG_OVR_CONDITIONAL_BREAKPOINT_DISABLED = "IMG_OBJS_CONDITIONAL_BREAKPOINT_DISABLED"; //$NON-NLS-1$

    public static final String IMG_OVR_SCOPED_BREAKPOINT = "IMG_OBJS_SCOPED_BREAKPOINT"; //$NON-NLS-1$

    public static final String IMG_OVR_SCOPED_BREAKPOINT_DISABLED = "IMG_OBJS_SCOPED_BREAKPOINT_DISABLED"; //$NON-NLS-1$

    public static final String IMG_OVR_UNCAUGHT_BREAKPOINT = "IMG_OBJS_UNCAUGHT_BREAKPOINT"; //$NON-NLS-1$

    public static final String IMG_OVR_UNCAUGHT_BREAKPOINT_DISABLED = "IMG_OBJS_UNCAUGHT_BREAKPOINT_DISABLED"; //$NON-NLS-1$

    public static final String IMG_OVR_CAUGHT_BREAKPOINT = "IMG_OBJS_CAUGHT_BREAKPOINT"; //$NON-NLS-1$

    public static final String IMG_OVR_CAUGHT_BREAKPOINT_DISABLED = "IMG_OBJS_CAUGHT_BREAKPOINT_DISABLED"; //$NON-NLS-1$

    public static final String IMG_OBJS_SNIPPET_EVALUATING = "IMG_OBJS_SNIPPET_EVALUATING"; //$NON-NLS-1$

    public static final String IMG_VIEW_ARGUMENTS_TAB = "IMG_VIEW_ARGUMENTS_TAB"; //$NON-NLS-1$

    public static final String IMG_OBJS_MONITOR = "IMG_OBJS_MONITOR"; //$NON-NLS-1$

    public static final String IMG_OBJS_CONTENDED_MONITOR = "IMG_OBJS_CONTENDED_MONITOR"; //$NON-NLS-1$

    public static final String IMG_OBJS_OWNED_MONITOR = "IMG_OBJS_OWNED_MONITOR"; //$NON-NLS-1$

    public static final String IMG_OVR_OWNED = "IMG_OVR_OWNED"; //$NON-NLS-1$

    public static final String IMG_OVR_OWNS_MONITOR = "IMG_OVR_OWNS_MONITOR"; //$NON-NLS-1$

    public static final String IMG_OVR_IN_CONTENTION = "IMG_OVR_IN_CONTENTION"; //$NON-NLS-1$

    public static final String IMG_OVR_IN_CONTENTION_FOR_MONITOR = "IMG_OVR_IN_CONTENTION_FOR_MONITOR"; //$NON-NLS-1$

    public static final String IMG_OVR_IN_DEADLOCK = "IMG_OVR_IN_DEADLOCK"; //$NON-NLS-1$

    public static final String IMG_OBJS_EXCEPTION_BRKPT_TYPE = "IMG_OBJS_EXCEPTION_BRKPT_TYPE"; //$NON-NLS-1$

    public static final String IMG_OBJS_LINE_BRKPT_TYPE = "IMG_OBJS_LINE_BRKPT_TYPE"; //$NON-NLS-1$

    public static final String IMG_OBJS_CLASSLOAD_BRKPT_TYPE = "IMG_OBJS_CLASSLOAD_BRKPT_TYPE"; //$NON-NLS-1$

    public static final String IMG_OBJS_WATCHPOINT_TYPE = "IMG_OBJS_WATCHPOINT_TYPE"; //$NON-NLS-1$

    public static final String IMG_OBJS_JSP_BRKPT_TYPE = "IMG_OBJS_JSP_BRKPT_TYPE"; //$NON-NLS-1$

    public static final String IMG_OBJS_METHOD_BRKPT_TYPE = "IMG_OBJS_METHOD_BRKPT_TYPE"; //$NON-NLS-1$

    public static final String IMG_OBJS_CLASSPATH = "IMG_OBJS_CLASSPATH"; //$NON-NLS-1$

    public static final String IMG_OVR_OUT_OF_SYNCH = "IMG_OVR_OUT_OF_SYNCH"; //$NON-NLS-1$

    public static final String IMG_OVR_MAY_BE_OUT_OF_SYNCH = "IMG_OVR_MAY_BE_OUT_OF_SYNCH"; //$NON-NLS-1$

    public static final String IMG_OVR_SYNCHRONIZED = "IMG_OVR_SYNCHRONIZED"; //$NON-NLS-1$

    public static final String IMG_WIZBAN_NEWSCRAPPAGE = "IMG_WIZBAN_NEWSCRAPPAGE"; //$NON-NLS-1$

    public static final String IMG_WIZBAN_LIBRARY = "IMG_WIZBAN_LIBRARY"; //$NON-NLS-1$

    public static final String IMG_TOOL_TERMSNIPPET = "IMG_TOOL_TERMSNIPPET"; //$NON-NLS-1$

    public static final String IMG_TOOL_TERMSNIPPET_HOVER = "IMG_TOOL_TERMSNIPPET_HOVER"; //$NON-NLS-1$

    public static final String IMG_TOOL_TERMSNIPPET_DISABLED = "IMG_TOOL_TERMSNIPPET_DISABLED"; //$NON-NLS-1$

    public static final String IMG_OBJ_JAVA_INSPECT_EXPRESSION = "IMG_OBJ_JAVA_INSPECT_EXPRESSION"; //$NON-NLS-1$

    public static final String IMG_OBJ_UNINTERPRETED_STACK_FRAME = "IMG_OBJ_UNINTERPRETED_STACK_FRAME"; //$NON-NLS-1$

    /*
     * Set of predefined Image Descriptors.
     */
    private static final String T_OBJ = "obj16/"; //$NON-NLS-1$

    private static final String T_OVR = "ovr16/"; //$NON-NLS-1$

    private static final String T_WIZBAN = "wizban/"; //$NON-NLS-1$

    private static final String T_EVIEW = "eview16/"; //$NON-NLS-1$

    private static final String T_DLCL = "dtool16/"; //$NON-NLS-1$

    private static final String T_ELCL = "etool16/"; //$NON-NLS-1$

    /**
     * Returns the image managed under the given key in this registry.
     * 
     * @param key
     *            the image's key
     * @return the image managed under the given key
     */
    public static Image get(final String key) {
        return getImageRegistry().get(key);
    }

    /**
     * Returns the <code>ImageDescriptor</code> identified by the given key, or
     * <code>null</code> if it does not exist.
     */
    public static ImageDescriptor getImageDescriptor(final String key) {
        return getImageRegistry().getDescriptor(key);
    }

    /*
     * Helper method to access the image registry from the JDIDebugUIPlugin
     * class.
     */
    /* package */static ImageRegistry getImageRegistry() {
        if (fgImageRegistry == null) {
            initializeImageRegistry();
        }
        return fgImageRegistry;
    }

    private static void initializeImageRegistry() {
        fgImageRegistry = new ImageRegistry(ErlideUIPlugin.getStandardDisplay());
        declareImages();
    }

    private static void declareImages() {
        declareRegistryImage(IMG_OBJS_EXCEPTION, T_OBJ + "jexception_obj.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OBJS_EXCEPTION_DISABLED, T_OBJ
                + "jexceptiond_obj.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OVR_BREAKPOINT_INSTALLED, T_OVR
                + "installed_ovr.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OVR_BREAKPOINT_INSTALLED_DISABLED, T_OVR
                + "installed_ovr_disabled.gif"); //$NON-NLS-1$

        declareRegistryImage(IMG_OBJS_LOCAL_VARIABLE, T_OBJ
                + "localvariable_obj.gif"); //$NON-NLS-1$

        declareRegistryImage(IMG_OVR_METHOD_BREAKPOINT_ENTRY, T_OVR
                + "entry_ovr.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OVR_METHOD_BREAKPOINT_ENTRY_DISABLED, T_OVR
                + "entry_ovr_disabled.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OVR_METHOD_BREAKPOINT_EXIT, T_OVR
                + "exit_ovr.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OVR_METHOD_BREAKPOINT_EXIT_DISABLED, T_OVR
                + "exit_ovr_disabled.gif"); //$NON-NLS-1$

        declareRegistryImage(IMG_OVR_CONDITIONAL_BREAKPOINT, T_OVR
                + "conditional_ovr.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OVR_CONDITIONAL_BREAKPOINT_DISABLED, T_OVR
                + "conditional_ovr_disabled.gif"); //$NON-NLS-1$

        declareRegistryImage(IMG_OVR_SCOPED_BREAKPOINT, T_OVR
                + "scoped_ovr.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OVR_SCOPED_BREAKPOINT_DISABLED, T_OVR
                + "scoped_ovr_disabled.gif"); //$NON-NLS-1$

        declareRegistryImage(IMG_OVR_UNCAUGHT_BREAKPOINT, T_OVR
                + "uncaught_ovr.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OVR_UNCAUGHT_BREAKPOINT_DISABLED, T_OVR
                + "uncaught_ovr_disabled.gif"); //$NON-NLS-1$

        declareRegistryImage(IMG_OVR_CAUGHT_BREAKPOINT, T_OVR
                + "caught_ovr.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OVR_CAUGHT_BREAKPOINT_DISABLED, T_OVR
                + "caught_ovr_disabled.gif"); //$NON-NLS-1$

        declareRegistryImage(IMG_OBJS_ERROR, T_OBJ + "jrtexception_obj.gif"); //$NON-NLS-1$

        declareRegistryImage(IMG_OBJS_SNIPPET_EVALUATING, T_OBJ
                + "jsbook_run_obj.gif"); //$NON-NLS-1$

        declareRegistryImage(IMG_VIEW_ARGUMENTS_TAB, T_EVIEW
                + "variable_tab.gif"); //$NON-NLS-1$

        declareRegistryImage(IMG_OVR_OUT_OF_SYNCH, T_OVR + "error_co.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OVR_MAY_BE_OUT_OF_SYNCH, T_OVR
                + "warning_co.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OVR_SYNCHRONIZED, T_OVR + "sync_ovr.gif"); //$NON-NLS-1$

        declareRegistryImage(IMG_OBJS_MONITOR, T_OBJ + "monitor_obj.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OVR_OWNED, T_OVR + "owned_ovr.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OVR_OWNS_MONITOR, T_OVR
                + "ownsmonitor_ovr.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OVR_IN_CONTENTION, T_OVR
                + "contention_ovr.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OVR_IN_CONTENTION_FOR_MONITOR, T_OVR
                + "contentionformonitor_ovr.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OVR_IN_DEADLOCK, T_OVR + "deadlock_ovr.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OBJS_CONTENDED_MONITOR, T_OBJ
                + "contended_monitor_obj.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OBJS_OWNED_MONITOR, T_OBJ
                + "owned_monitor_obj.gif"); //$NON-NLS-1$

        declareRegistryImage(IMG_WIZBAN_NEWSCRAPPAGE, T_WIZBAN
                + "newsbook_wiz.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_WIZBAN_LIBRARY, T_WIZBAN + "library_wiz.gif"); //$NON-NLS-1$

        declareRegistryImage(IMG_TOOL_TERMSNIPPET, T_ELCL + "term_sbook.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_TOOL_TERMSNIPPET_HOVER, T_ELCL
                + "term_sbook.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_TOOL_TERMSNIPPET_DISABLED, T_DLCL
                + "term_sbook.gif"); //$NON-NLS-1$

        declareRegistryImage(IMG_OBJ_JAVA_INSPECT_EXPRESSION, T_OBJ
                + "insp_sbook.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OBJS_CLASSPATH, T_OBJ + "classpath_obj.gif"); //$NON-NLS-1$

        declareRegistryImage(IMG_OBJS_EXCEPTION_BRKPT_TYPE, T_OBJ
                + "jexcept_obj.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OBJS_LINE_BRKPT_TYPE, T_OBJ + "jline_obj.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OBJS_CLASSLOAD_BRKPT_TYPE, T_OBJ
                + "jload_obj.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OBJS_METHOD_BRKPT_TYPE, T_OBJ
                + "jmeth_obj.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OBJS_JSP_BRKPT_TYPE, T_OBJ
                + "jspbrkpt_obj.gif"); //$NON-NLS-1$
        declareRegistryImage(IMG_OBJS_WATCHPOINT_TYPE, T_OBJ + "jwatch_obj.gif"); //$NON-NLS-1$

        declareRegistryImage(IMG_OBJ_UNINTERPRETED_STACK_FRAME, T_OBJ
                + "uninterpreted_stckframe_obj.gif"); //$NON-NLS-1$

    }

    /**
     * Declare an Image in the registry table.
     * 
     * @param key
     *            The key to use when registering the image
     * @param path
     *            The path where the image can be found. This path is relative
     *            to where this plugin class is found (i.e. typically the
     *            packages directory)
     */
    private final static void declareRegistryImage(final String key,
            final String path) {
        ImageDescriptor desc = ImageDescriptor.getMissingImageDescriptor();
        try {
            desc = ImageDescriptor.createFromURL(makeIconFileURL(path));
        } catch (final MalformedURLException me) {
            // ErlideUIPlugin.log(me);
        }
        fgImageRegistry.put(key, desc);
    }

    private static URL makeIconFileURL(final String iconPath)
            throws MalformedURLException {
        if (ICON_BASE_URL == null) {
            throw new MalformedURLException();
        }

        return new URL(ICON_BASE_URL, iconPath);
    }

}
