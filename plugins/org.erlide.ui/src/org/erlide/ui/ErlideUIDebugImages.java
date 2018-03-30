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
    private static URL ICON_BASE_URL;

    static {
        final String pathSuffix = "icons/full/"; //$NON-NLS-1$
        ErlideUIDebugImages.ICON_BASE_URL = ErlideUIPlugin.getDefault().getBundle().getEntry(pathSuffix);
    }

    // The plugin registry
    private static ImageRegistry fgImageRegistry;

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
        return ErlideUIDebugImages.getImageRegistry().get(key);
    }

    /**
     * Returns the <code>ImageDescriptor</code> identified by the given key, or
     * <code>null</code> if it does not exist.
     */
    public static ImageDescriptor getImageDescriptor(final String key) {
        return ErlideUIDebugImages.getImageRegistry().getDescriptor(key);
    }

    /*
     * Helper method to access the image registry from the JDIDebugUIPlugin
     * class.
     */
    /* package */static ImageRegistry getImageRegistry() {
        if (ErlideUIDebugImages.fgImageRegistry == null) {
            ErlideUIDebugImages.initializeImageRegistry();
        }
        return ErlideUIDebugImages.fgImageRegistry;
    }

    private static void initializeImageRegistry() {
        ErlideUIDebugImages.fgImageRegistry = new ImageRegistry(ErlideUIPlugin.getStandardDisplay());
        ErlideUIDebugImages.declareImages();
    }

    private static void declareImages() {
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OBJS_EXCEPTION, ErlideUIDebugImages.T_OBJ + "jexception_obj.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OBJS_EXCEPTION_DISABLED, ErlideUIDebugImages.T_OBJ + "jexceptiond_obj.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OVR_BREAKPOINT_INSTALLED, ErlideUIDebugImages.T_OVR + "installed_ovr.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OVR_BREAKPOINT_INSTALLED_DISABLED,
                ErlideUIDebugImages.T_OVR + "installed_ovr_disabled.gif"); //$NON-NLS-1$

        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OBJS_LOCAL_VARIABLE, ErlideUIDebugImages.T_OBJ + "localvariable_obj.gif"); //$NON-NLS-1$

        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OVR_METHOD_BREAKPOINT_ENTRY, ErlideUIDebugImages.T_OVR + "entry_ovr.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OVR_METHOD_BREAKPOINT_ENTRY_DISABLED,
                ErlideUIDebugImages.T_OVR + "entry_ovr_disabled.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OVR_METHOD_BREAKPOINT_EXIT, ErlideUIDebugImages.T_OVR + "exit_ovr.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OVR_METHOD_BREAKPOINT_EXIT_DISABLED,
                ErlideUIDebugImages.T_OVR + "exit_ovr_disabled.gif"); //$NON-NLS-1$

        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OVR_CONDITIONAL_BREAKPOINT,
                ErlideUIDebugImages.T_OVR + "conditional_ovr.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OVR_CONDITIONAL_BREAKPOINT_DISABLED,
                ErlideUIDebugImages.T_OVR + "conditional_ovr_disabled.gif"); //$NON-NLS-1$

        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OVR_SCOPED_BREAKPOINT, ErlideUIDebugImages.T_OVR + "scoped_ovr.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OVR_SCOPED_BREAKPOINT_DISABLED,
                ErlideUIDebugImages.T_OVR + "scoped_ovr_disabled.gif"); //$NON-NLS-1$

        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OVR_UNCAUGHT_BREAKPOINT, ErlideUIDebugImages.T_OVR + "uncaught_ovr.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OVR_UNCAUGHT_BREAKPOINT_DISABLED,
                ErlideUIDebugImages.T_OVR + "uncaught_ovr_disabled.gif"); //$NON-NLS-1$

        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OVR_CAUGHT_BREAKPOINT, ErlideUIDebugImages.T_OVR + "caught_ovr.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OVR_CAUGHT_BREAKPOINT_DISABLED,
                ErlideUIDebugImages.T_OVR + "caught_ovr_disabled.gif"); //$NON-NLS-1$

        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OBJS_ERROR, ErlideUIDebugImages.T_OBJ + "jrtexception_obj.gif"); //$NON-NLS-1$

        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OBJS_SNIPPET_EVALUATING, ErlideUIDebugImages.T_OBJ + "jsbook_run_obj.gif"); //$NON-NLS-1$

        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_VIEW_ARGUMENTS_TAB, ErlideUIDebugImages.T_EVIEW + "variable_tab.gif"); //$NON-NLS-1$

        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OVR_OUT_OF_SYNCH, ErlideUIDebugImages.T_OVR + "error_co.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OVR_MAY_BE_OUT_OF_SYNCH, ErlideUIDebugImages.T_OVR + "warning_co.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OVR_SYNCHRONIZED, ErlideUIDebugImages.T_OVR + "sync_ovr.gif"); //$NON-NLS-1$

        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OBJS_MONITOR, ErlideUIDebugImages.T_OBJ + "monitor_obj.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OVR_OWNED, ErlideUIDebugImages.T_OVR + "owned_ovr.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OVR_OWNS_MONITOR, ErlideUIDebugImages.T_OVR + "ownsmonitor_ovr.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OVR_IN_CONTENTION, ErlideUIDebugImages.T_OVR + "contention_ovr.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OVR_IN_CONTENTION_FOR_MONITOR,
                ErlideUIDebugImages.T_OVR + "contentionformonitor_ovr.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OVR_IN_DEADLOCK, ErlideUIDebugImages.T_OVR + "deadlock_ovr.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OBJS_CONTENDED_MONITOR,
                ErlideUIDebugImages.T_OBJ + "contended_monitor_obj.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OBJS_OWNED_MONITOR, ErlideUIDebugImages.T_OBJ + "owned_monitor_obj.gif"); //$NON-NLS-1$

        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_WIZBAN_NEWSCRAPPAGE, ErlideUIDebugImages.T_WIZBAN + "newsbook_wiz.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_WIZBAN_LIBRARY, ErlideUIDebugImages.T_WIZBAN + "library_wiz.gif"); //$NON-NLS-1$

        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_TOOL_TERMSNIPPET, ErlideUIDebugImages.T_ELCL + "term_sbook.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_TOOL_TERMSNIPPET_HOVER, ErlideUIDebugImages.T_ELCL + "term_sbook.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_TOOL_TERMSNIPPET_DISABLED, ErlideUIDebugImages.T_DLCL + "term_sbook.gif"); //$NON-NLS-1$

        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OBJ_JAVA_INSPECT_EXPRESSION, ErlideUIDebugImages.T_OBJ + "insp_sbook.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OBJS_CLASSPATH, ErlideUIDebugImages.T_OBJ + "classpath_obj.gif"); //$NON-NLS-1$

        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OBJS_EXCEPTION_BRKPT_TYPE, ErlideUIDebugImages.T_OBJ + "jexcept_obj.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OBJS_LINE_BRKPT_TYPE, ErlideUIDebugImages.T_OBJ + "jline_obj.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OBJS_CLASSLOAD_BRKPT_TYPE, ErlideUIDebugImages.T_OBJ + "jload_obj.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OBJS_METHOD_BRKPT_TYPE, ErlideUIDebugImages.T_OBJ + "jmeth_obj.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OBJS_JSP_BRKPT_TYPE, ErlideUIDebugImages.T_OBJ + "jspbrkpt_obj.gif"); //$NON-NLS-1$
        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OBJS_WATCHPOINT_TYPE, ErlideUIDebugImages.T_OBJ + "jwatch_obj.gif"); //$NON-NLS-1$

        ErlideUIDebugImages.declareRegistryImage(ErlideUIDebugImages.IMG_OBJ_UNINTERPRETED_STACK_FRAME,
                ErlideUIDebugImages.T_OBJ + "uninterpreted_stckframe_obj.gif"); //$NON-NLS-1$

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
    private static final void declareRegistryImage(final String key, final String path) {
        ImageDescriptor desc = ImageDescriptor.getMissingImageDescriptor();
        try {
            desc = ImageDescriptor.createFromURL(ErlideUIDebugImages.makeIconFileURL(path));
        } catch (final MalformedURLException me) {
            // ErlideUIPlugin.log(me);
        }
        ErlideUIDebugImages.fgImageRegistry.put(key, desc);
    }

    private static URL makeIconFileURL(final String iconPath)
            throws MalformedURLException {
        if (ErlideUIDebugImages.ICON_BASE_URL == null) {
            throw new MalformedURLException();
        }

        return new URL(ErlideUIDebugImages.ICON_BASE_URL, iconPath);
    }

}
