/*******************************************************************************
 * Copyright (c) 2011 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.internal.ErlideUIPlugin;

public enum ErlideImage {

    // @formatter:off
    ALPHAB_SORT(ErlideImagePrefix.T_ELCL, "alphab_sort_co.gif"),
    TEST_RUNNING(ErlideImagePrefix.T_OBJ, "searchm_obj.gif"),
    TEST_SUCCEEDED(ErlideImagePrefix.T_OBJ, "methpub_obj.gif"),
    TEST_FAILED(ErlideImagePrefix.T_OBJ, "methpri_obj.gif"),
    TEST_SKIPPED(ErlideImagePrefix.T_OBJ, "methpro_obj.gif"),
    CLEAR(ErlideImagePrefix.T_ELCL, "clear_co.gif"),
    EXPORTED_SORT(ErlideImagePrefix.T_ELCL, "exported_sort_co.gif"),
    HIDE_LOCAL_FUNCTIONS(ErlideImagePrefix.T_ELCL, "hide_local_functions.gif"),
    HIDE_MACRO_RECORD_DEFS(ErlideImagePrefix.T_ELCL,
            "hide_macro_record_defs.gif"),
    HIDE_ATTRIBUTES(ErlideImagePrefix.T_ELCL, "hide_attributes.gif"),
    FUNCTION_EXPORTED(ErlideImagePrefix.T_OBJ, "methpub_obj.gif"),
    FUNCTION_DEFAULT(ErlideImagePrefix.T_OBJ, "methpri_obj.gif"),
    FUNCTION_CLAUSE(ErlideImagePrefix.T_OBJ, "methpro_obj.gif"),
    RECORD_DEF(ErlideImagePrefix.T_OBJ, "typevariable_obj.gif"),
    RECORD_FIELD(ErlideImagePrefix.T_OBJ, "typevariable_obj.gif"),
    MACRO_DEF(ErlideImagePrefix.T_OBJ, "typevariable_obj.gif"),
    TYPESPEC_DEF(ErlideImagePrefix.T_OBJ, "typevariable_obj.gif"),
    SRC_FOLDER(ErlideImagePrefix.T_OBJ, "erlang_src_folder_obj.gif"),
    ATTRIBUTE(ErlideImagePrefix.T_OBJ, "field_public_obj.gif"),
    EXPORT(ErlideImagePrefix.T_OBJ, "field_public_obj.gif"),
    EXTERNAL(ErlideImagePrefix.T_OBJ, "external_ref.gif"),
    IMPORT(ErlideImagePrefix.T_OBJ, "field_public_obj.gif"),
    OVR_WARNING(ErlideImagePrefix.T_OVR, "warning_co.gif"),
    OVR_ERROR(ErlideImagePrefix.T_OVR, "error_co.gif"),
    ERLANG_SEARCH_RESULTS(ErlideImagePrefix.T_OBJ, "erlang_search_results.gif"),
    MODULE(ErlideImagePrefix.T_OBJ, "erlang_srcFile.png"),
    MODULE_RESOURCE(ErlideImagePrefix.T_OBJ, "erlang_srcFileExt.gif"),
    UNKNOWN(ErlideImagePrefix.T_OBJ, "unknown_obj.gif"),
    OBJS_EDOCTAG(ErlideImagePrefix.T_OBJ, "jdoc_tag_obj.gif");
    // @formatter:on

    private String prefix;
    private String path;

    private static volatile ImageRegistry registry;

    private static URL fgIconBaseURL = null;
    static {
        fgIconBaseURL = ErlideUIPlugin.getDefault().getBundle()
                .getEntry("/icons/full/"); //$NON-NLS-1$
    }

    private ErlideImage(final ErlideImagePrefix prefix, final String path) {
        this.prefix = prefix.getPrefix();
        this.path = path;
    }

    public URL url() throws MalformedURLException {
        return makeIconFileURL(prefix, path);
    }

    public Image getImage() {
        return getRegistry().get(name());
    }

    public ImageDescriptor getDescriptor() {
        return getRegistry().getDescriptor(name());
    }

    public static boolean isInstalled() {
        return registry != null;
    }

    public static synchronized void dispose() {
        registry.dispose();
        registry = null;
    }

    private static ImageRegistry getRegistry() {
        if (registry == null) {
            synchronized (ErlideImage.class) {
                if (registry == null) {
                    registry = new ImageRegistry(Display.getDefault());
                    registerAllImages();
                }
            }
        }
        return registry;
    }

    private static void registerAllImages() {
        for (final ErlideImage key : values()) {
            ImageDescriptor descriptor;
            try {
                final URL url = key.url();
                descriptor = ImageDescriptor.createFromURL(url);
            } catch (final MalformedURLException e) {
                descriptor = ImageDescriptor.getMissingImageDescriptor();
            }
            registry.put(key.name(), descriptor);
        }
    }

    private static URL makeIconFileURL(final String prefix, final String name)
            throws MalformedURLException {
        if (fgIconBaseURL == null) {
            throw new MalformedURLException();
        }

        final StringBuilder buffer = new StringBuilder(prefix);
        buffer.append('/');
        buffer.append(name);
        return new URL(fgIconBaseURL, buffer.toString());
    }

    /**
     * Sets the three image descriptors for enabled, disabled, and hovered to an
     * action. The actions are retrieved from the *tool16 folders.
     * 
     * @param action
     *            the action
     * @param iconName
     *            the icon name
     */
    public static void setToolImageDescriptors(final IAction action,
            final String iconName) {
        setImageDescriptors(action, "tool16", iconName);
    }

    /**
     * Sets the three image descriptors for enabled, disabled, and hovered to an
     * action. The icons are retrieved from the *lcl16 folders.
     * 
     * @param action
     *            the action
     * @param iconName
     *            the icon name
     */
    public static void setLocalImageDescriptors(final IAction action,
            final String iconName) {
        setImageDescriptors(action, "lcl16", iconName);
    }

    private static void setImageDescriptors(final IAction action,
            final String type, final String relPath) {

        try {
            final ImageDescriptor id = ImageDescriptor
                    .createFromURL(makeIconFileURL("d" + type, relPath));
            if (id != null) {
                action.setDisabledImageDescriptor(id);
            }
        } catch (final MalformedURLException e) {
            ErlLogger.warn(e);
        }

        try {
            final ImageDescriptor descriptor = ImageDescriptor
                    .createFromURL(makeIconFileURL("e" + type, relPath));
            action.setHoverImageDescriptor(descriptor);
            action.setImageDescriptor(descriptor);
        } catch (final MalformedURLException e) {
            ErlLogger.warn(e);
        }
    }

}
