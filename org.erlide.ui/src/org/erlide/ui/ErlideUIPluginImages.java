/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.graphics.Image;

/**
 * 
 * 
 * @author Vlad Dumitrescu [vladdu55 at gmail dot com]
 */
public class ErlideUIPluginImages {

    private static final String NAME_PREFIX = "org.erlide.ui."; //$NON-NLS-1$

    private static final int NAME_PREFIX_LENGTH = NAME_PREFIX.length();

    private static URL fgIconBaseURL = null;

    // Determine display depth. If depth > 4 then we use high color images.
    // Otherwise low
    // color images are used
    static {
        fgIconBaseURL = ErlideUIPlugin.getDefault().getBundle()
                .getEntry("/icons/full/"); //$NON-NLS-1$
    }

    // The plug-in registry
    private static ImageRegistry fgImageRegistry = null;

    private static HashMap<String, ImageDescriptor> fgAvoidSWTErrorMap = null;

    private static final String T_OBJ = "obj16"; //$NON-NLS-1$

    private static final String T_OVR = "ovr16"; //$NON-NLS-1$

    // private static final String T_WIZBAN = "wizban"; //$NON-NLS-1$
    private static final String T_ELCL = "elcl16"; //$NON-NLS-1$
    // private static final String T_DLCL = "dlcl16"; //$NON-NLS-1$
    // private static final String T_ETOOL = "etool16"; //$NON-NLS-1$
    // private static final String T_EVIEW = "eview16"; //$NON-NLS-1$

    public static final String IMG_ALPHAB_SORT = NAME_PREFIX
            + "alphab_sort_co.gif"; //$NON-NLS-1$

    public static final String IMG_EXPORTED_SORT = NAME_PREFIX
            + "exported_sort_co.gif"; //$NON-NLS-1$

    public static final String IMG_FUNCTION_EXPORTED = NAME_PREFIX
            + "methpub_obj.gif"; //$NON-NLS-1$

    public static final String IMG_FUNCTION_DEFAULT = NAME_PREFIX
            + "methpri_obj.gif"; //$NON-NLS-1$

    public static final String IMG_FUNCTION_CLAUSE = NAME_PREFIX
            + "methpro_obj.gif"; //$NON-NLS-1$

    public static final String IMG_ATTRIBUTE = NAME_PREFIX
            + "field_public_obj.gif"; //$NON-NLS-1$

    public static final String IMG_EXPORT = NAME_PREFIX
            + "field_public_obj.gif"; //$NON-NLS-1$

    public static final String IMG_IMPORT = NAME_PREFIX
            + "field_public_obj.gif"; //$NON-NLS-1$

    public static final String IMG_RECORD_DEF = NAME_PREFIX
            + "typevariable_obj.gif"; //$NON-NLS-1$

    public static final String IMG_RECORD_FIELD = NAME_PREFIX
            + "typevariable_obj.gif"; //$NON-NLS-1$

    public static final String IMG_MACRO_DEF = NAME_PREFIX
            + "typevariable_obj.gif"; //$NON-NLS-1$

    public static final String IMG_TYPESPEC_DEF = NAME_PREFIX
            + "typevariable_obj.gif"; //$NON-NLS-1$

    public static final String IMG_SRC_FOLDER = NAME_PREFIX
            + "erlang_src_folder_obj.gif"; //$NON-NLS-1$

    public static final String IMG_EXTERNAL = NAME_PREFIX + "external_ref.gif"; //$NON-NLS-1$

    public static final ImageDescriptor DESC_ALPHAB_SORT = createManaged(
            T_ELCL, IMG_ALPHAB_SORT);

    public static final ImageDescriptor DESC_EXPORTED_SORT = createManaged(
            T_ELCL, IMG_EXPORTED_SORT);

    public static final String IMG_HIDE_LOCAL_FUNCTIONS = NAME_PREFIX
            + "hide_local_functions.gif"; //$NON-NLS-1$

    public static final ImageDescriptor DESC_HIDE_LOCAL_FUNCTIONS = createManaged(
            T_ELCL, IMG_HIDE_LOCAL_FUNCTIONS);

    public static final String IMG_HIDE_MACRO_RECORD_DEFS = NAME_PREFIX
            + "hide_macro_record_defs.gif"; //$NON-NLS-1$

    public static final ImageDescriptor DESC_HIDE_MACRO_RECORD_DEFS = createManaged(
            T_ELCL, IMG_HIDE_MACRO_RECORD_DEFS);

    public static final String IMG_HIDE_ATTRIBUTES = NAME_PREFIX
            + "hide_attributes.gif"; //$NON-NLS-1$

    public static final ImageDescriptor DESC_HIDE_ATTRIBUTES = createManaged(
            T_ELCL, IMG_HIDE_ATTRIBUTES);

    public static final ImageDescriptor DESC_FUNCTION_EXPORTED = createManaged(
            T_OBJ, IMG_FUNCTION_EXPORTED);

    public static final ImageDescriptor DESC_FUNCTION_DEFAULT = createManaged(
            T_OBJ, IMG_FUNCTION_DEFAULT);

    public static final ImageDescriptor DESC_FUNCTION_CLAUSE = createManaged(
            T_OBJ, IMG_FUNCTION_CLAUSE);

    public static final ImageDescriptor DESC_RECORD_DEF = createManaged(T_OBJ,
            IMG_RECORD_DEF);

    public static final ImageDescriptor DESC_RECORD_FIELD = createManaged(
            T_OBJ, IMG_RECORD_FIELD);

    public static final ImageDescriptor DESC_MACRO_DEF = createManaged(T_OBJ,
            IMG_MACRO_DEF);

    public static final ImageDescriptor DESC_TYPESPEC_DEF = createManaged(
            T_OBJ, IMG_TYPESPEC_DEF);

    public static final ImageDescriptor DESC_SRC_FOLDER = createManaged(T_OBJ,
            IMG_SRC_FOLDER);

    public static final ImageDescriptor DESC_ATTRIBUTE = createManaged(T_OBJ,
            IMG_ATTRIBUTE);

    public static final ImageDescriptor DESC_EXPORT = createManaged(T_OBJ,
            IMG_EXPORT);

    public static final ImageDescriptor DESC_EXTERNAL = createManaged(T_OBJ,
            IMG_EXTERNAL);

    public static final ImageDescriptor DESC_IMPORT = createManaged(T_OBJ,
            IMG_IMPORT);

    public static final ImageDescriptor DESC_OVR_WARNING = create(T_OVR,
            "warning_co.gif");

    public static final ImageDescriptor DESC_OVR_ERROR = create(T_OVR,
            "error_co.gif");

    public static final String IMG_MODULE = NAME_PREFIX + "erlang_srcFile.png";

    public static final String IMG_MODULE_RESOURCE = NAME_PREFIX
            + "erlang_srcFileExt.gif";

    public static final ImageDescriptor DESC_MODULE = createManaged(T_OBJ,
            IMG_MODULE);

    public static final ImageDescriptor DESC_MODULE_RESOURCE = createManaged(
            T_OBJ, IMG_MODULE_RESOURCE);

    public static final String IMG_UNKNOWN = NAME_PREFIX + "unknown_obj.gif"; //$NON-NLS-1$

    public static final ImageDescriptor DESC_UNKNOWN = createManaged(T_OBJ,
            IMG_UNKNOWN);

    public static final String IMG_OBJS_EDOCTAG = NAME_PREFIX
            + "jdoc_tag_obj.gif"; //$NON-NLS-1$

    public static final ImageDescriptor DESC_OBJS_EDOCTAG = createManaged(
            T_OBJ, IMG_OBJS_EDOCTAG);

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
     * Returns the image descriptor for the given key in this registry. Might be
     * called in a non-UI thread.
     * 
     * @param key
     *            the image's key
     * @return the image descriptor for the given key
     */
    public static ImageDescriptor getDescriptor(final String key) {
        if (fgImageRegistry == null) {
            return fgAvoidSWTErrorMap.get(key);
        }
        return getImageRegistry().getDescriptor(key);
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
     * action. The actions are retrieved from the *lcl16 folders.
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

    /*
     * Helper method to access the image registry from the JavaPlugin class.
     */
    static ImageRegistry getImageRegistry() {
        if (fgImageRegistry == null) {
            final ImageRegistry reg = new ImageRegistry();
            for (final Object element : fgAvoidSWTErrorMap.keySet()) {
                final String key = (String) element;
                reg.put(key, fgAvoidSWTErrorMap.get(key));
            }
            fgAvoidSWTErrorMap = null;
            fgImageRegistry = reg;
        }
        return fgImageRegistry;
    }

    // ---- Helper methods to access icons on the file system
    // --------------------------------------

    private static void setImageDescriptors(final IAction action,
            final String type, final String relPath) {

        try {
            final ImageDescriptor id = ImageDescriptor
                    .createFromURL(makeIconFileURL("d" + type, relPath)); //$NON-NLS-1$
            if (id != null) {
                action.setDisabledImageDescriptor(id);
            }
        } catch (final MalformedURLException e) {
        }

        /*
         * try { ImageDescriptor id=
         * ImageDescriptor.createFromURL(makeIconFileURL("c" + type, relPath));
         * //$NON-NLS-1$ if (id != null) action.setHoverImageDescriptor(id); }
         * catch (MalformedURLException e) { }
         */

        final ImageDescriptor descriptor = create("e" + type, relPath); //$NON-NLS-1$
        action.setHoverImageDescriptor(descriptor);
        action.setImageDescriptor(descriptor);
    }

    private static ImageDescriptor createManaged(final String prefix,
            final String name) {
        try {
            final ImageDescriptor result = ImageDescriptor
                    .createFromURL(makeIconFileURL(prefix,
                            name.substring(NAME_PREFIX_LENGTH)));
            if (fgAvoidSWTErrorMap == null) {
                fgAvoidSWTErrorMap = new HashMap<String, ImageDescriptor>();
            }
            fgAvoidSWTErrorMap.put(name, result);
            if (fgImageRegistry != null) {
                ErlideUIPlugin
                        .logErrorMessage("Image registry already defined");
            }
            return result;
        } catch (final MalformedURLException e) {
            return ImageDescriptor.getMissingImageDescriptor();
        }
    }

    // private static ImageDescriptor createManaged(String prefix, String name,
    // String
    // key)
    // {
    // try
    // {
    // ImageDescriptor result =
    // ImageDescriptor.createFromURL(makeIconFileURL(prefix, name
    // .substring(NAME_PREFIX_LENGTH)));
    // if (fgAvoidSWTErrorMap == null)
    // {
    // fgAvoidSWTErrorMap = new HashMap();
    // }
    // fgAvoidSWTErrorMap.put(key, result);
    // if (fgImageRegistry != null)
    // {
    // ErlideUIPlugin.logErrorMessage("Image registry already defined");
    //
    // }
    // return result;
    // } catch (MalformedURLException e)
    // {
    // return ImageDescriptor.getMissingImageDescriptor ();
    // }
    // }

    private static ImageDescriptor create(final String prefix, final String name) {
        try {
            return ImageDescriptor.createFromURL(makeIconFileURL(prefix, name));
        } catch (final MalformedURLException e) {
            return ImageDescriptor.getMissingImageDescriptor();
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

}
