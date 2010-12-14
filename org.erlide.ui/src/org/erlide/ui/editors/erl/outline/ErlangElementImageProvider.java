/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.editors.erl.outline;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlElement.Kind;
import org.erlide.core.erlang.IErlFolder;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlModel;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.ErlideUIPluginImages;
import org.erlide.ui.util.ImageDescriptorRegistry;

/**
 * Default strategy of the Erlang plugin for the construction of Erlang element
 * icons.
 */
@SuppressWarnings("unused")
public class ErlangElementImageProvider {

    /**
     * Flags for the ErlangImageLabelProvider: Generate images with overlays.
     */
    public static final int OVERLAY_ICONS = 0x1;

    /**
     * Generate small sized images.
     */
    public static final int SMALL_ICONS = 0x2;

    /**
     * Use the 'light' style for rendering types.
     */
    public static final int LIGHT_TYPE_ICONS = 0x4;

    public static final Point SMALL_SIZE = new Point(16, 16);

    public static final Point BIG_SIZE = new Point(22, 16);

    private static ImageDescriptor DESC_OBJ_PROJECT_CLOSED;

    private static ImageDescriptor DESC_OBJ_PROJECT;
    {
        final ISharedImages images = ErlideUIPlugin.getDefault().getWorkbench()
                .getSharedImages();
        DESC_OBJ_PROJECT_CLOSED = images
                .getImageDescriptor(IDE.SharedImages.IMG_OBJ_PROJECT_CLOSED);
        DESC_OBJ_PROJECT = images
                .getImageDescriptor(IDE.SharedImages.IMG_OBJ_PROJECT);
    }

    private ImageDescriptorRegistry fRegistry;

    public ErlangElementImageProvider() {
        fRegistry = null; // lazy initialization
    }

    /**
     * Returns the icon for a given element. The icon depends on the element
     * type and element properties. If configured, overlay icons are constructed
     * for <code>ISourceReference</code>s.
     * 
     * @param flags
     *            Flags as defined by the ErlangImageLabelProvider
     */
    public Image getImageLabel(final Object element, final int flags) {
        return getImageLabel(computeDescriptor(element, flags));
    }

    public Image getImageLabel(final ImageDescriptor descriptor) {
        if (descriptor == null) {
            return null;
        }
        return getRegistry().get(descriptor);
    }

    private ImageDescriptorRegistry getRegistry() {
        if (fRegistry == null) {
            fRegistry = ErlideUIPlugin.getImageDescriptorRegistry();
        }
        return fRegistry;
    }

    private ImageDescriptor computeDescriptor(final Object element,
            final int flags) {
        if (element instanceof IErlElement) {
            return getErlImageDescriptor((IErlElement) element, flags);
        } else if (element instanceof IFile) {
            final IFile file = (IFile) element;
            if ("erl".equals(file.getFileExtension())) { //$NON-NLS-1$
                return getErlResourceImageDescriptor(file, flags);
                // image for a CU not on the build path
            }
            return getWorkbenchImageDescriptor(file, flags);
        } else if (element instanceof IFolder) {
            final IErlModel model = ErlangCore.getModel();
            final IErlFolder ef = (IErlFolder) model
                    .findElement((IResource) element);
            if (ef != null && ef.isOnSourcePath()) {
                final ImageDescriptor desc = getErlImageDescriptor(ef, flags);
            }
        } else if (element instanceof IAdaptable) {
            return getWorkbenchImageDescriptor((IAdaptable) element, flags);
        }
        return null;
    }

    private static boolean showOverlayIcons(final int flags) {
        return (flags & OVERLAY_ICONS) != 0;
    }

    private static boolean useSmallSize(final int flags) {
        return (flags & SMALL_ICONS) != 0;
    }

    private static boolean useLightIcons(final int flags) {
        return (flags & LIGHT_TYPE_ICONS) != 0;
    }

    /**
     * Returns an image descriptor for a module not on the class path. The
     * descriptor includes overlays, if specified.
     */
    public ImageDescriptor getErlResourceImageDescriptor(final IFile file,
            final int flags) {
        final Point size = useSmallSize(flags) ? SMALL_SIZE : BIG_SIZE;
        return new ErlangElementImageDescriptor(
                ErlideUIPluginImages.DESC_MODULE_RESOURCE, 0, size);
    }

    /**
     * Returns an image descriptor for an erlang element. The descriptor
     * includes overlays, if specified.
     */
    static public ImageDescriptor getErlImageDescriptor(
            final IErlElement element, final int flags) {
        final int adornmentFlags = 0; // computeAdornmentFlags(element,
        // flags);
        final Point size = useSmallSize(flags) ? SMALL_SIZE : BIG_SIZE;
        return new ErlangElementImageDescriptor(getBaseImageDescriptor(element,
                flags), adornmentFlags, size);
    }

    /**
     * Returns an image descriptor for a IAdaptable. The descriptor includes
     * overlays, if specified (only error ticks apply). Returns
     * <code>null</code> if no image could be found.
     */
    public ImageDescriptor getWorkbenchImageDescriptor(
            final IAdaptable adaptable, final int flags) {
        final IWorkbenchAdapter wbAdapter = (IWorkbenchAdapter) adaptable
                .getAdapter(IWorkbenchAdapter.class);
        if (wbAdapter == null) {
            return null;
        }
        final ImageDescriptor descriptor = wbAdapter
                .getImageDescriptor(adaptable);
        if (descriptor == null) {
            return null;
        }

        final Point size = useSmallSize(flags) ? SMALL_SIZE : BIG_SIZE;
        return new ErlangElementImageDescriptor(descriptor, 0, size);
    }

    // ---- Computation of base image key
    // -------------------------------------------------

    /**
     * Returns an image descriptor for an Erlang element. This is the base
     * image, no overlays.
     */
    static public ImageDescriptor getBaseImageDescriptor(
            final IErlElement element, final int renderFlags) {
        if (element instanceof IErlFunction) {
            final IErlFunction fun = (IErlFunction) element;
            if (fun.isExported()) {
                return ErlideUIPluginImages.DESC_FUNCTION_EXPORTED;
            }
        }
        return getImageDescriptionFromKind(element.getKind());
    }

    public static ImageDescriptor getImageDescriptionFromKind(final Kind kind) {
        switch (kind) {
        case ATTRIBUTE:
            return ErlideUIPluginImages.DESC_ATTRIBUTE;
        case CLAUSE:
            return ErlideUIPluginImages.DESC_FUNCTION_CLAUSE;
        case COMMENT:
        case ERROR:
        case HEADERCOMMENT:
        case MODEL:
        case PROJECT:
            return ErlideUIPluginImages.DESC_UNKNOWN;
        case EXPORT:
            return ErlideUIPluginImages.DESC_EXPORT;
        case EXPORTFUNCTION:
            return ErlideUIPluginImages.DESC_FUNCTION_EXPORTED;
        case FOLDER:
            return ErlideUIPluginImages.DESC_SRC_FOLDER;
        case FUNCTION:
            return ErlideUIPluginImages.DESC_FUNCTION_DEFAULT;
        case EXTERNAL:
            return ErlideUIPluginImages.DESC_EXTERNAL;
        case IMPORT:
            return ErlideUIPluginImages.DESC_IMPORT;
        case MACRO_DEF:
            return ErlideUIPluginImages.DESC_MACRO_DEF;
        case MODULE:
            return ErlideUIPluginImages.DESC_MODULE;
        case RECORD_DEF:
            return ErlideUIPluginImages.DESC_RECORD_DEF;
        case TYPESPEC:
            return ErlideUIPluginImages.DESC_TYPESPEC_DEF;
        case RECORD_FIELD:
            return ErlideUIPluginImages.DESC_RECORD_FIELD;
        }
        return ErlideUIPluginImages.DESC_UNKNOWN;
    }

    public void dispose() {
    }

    // ---- Methods to compute the adornments flags
    // ---------------------------------

    // protected int computeAdornmentFlags(final IErlElement element,
    // final int renderFlags) {
    // int flags = 0;
    // if (showOverlayIcons(renderFlags) && element instanceof IErlFunction) {
    // try {
    // final IErlFunction member = (IErlFunction) element;
    //
    // if (member.isExported()) {
    // flags |= ErlangElementImageDescriptor.EXPORTED;
    // }
    //
    // } catch (final Exception e) {
    // // do nothing. Can't compute runnable adornment or get flags
    // }
    // }
    // if (element instanceof ISourceReference) {
    // final ISourceReference sr = (ISourceReference) element;
    // try {
    // flags |= ProblemsLabelDecorator.getErrorTicksFromMarkers(
    // element.getResource(), IResource.DEPTH_INFINITE, sr);
    // if (element instanceof IParent) {
    // final IParent p = (IParent) element;
    // for (final IErlElement e : p.getChildren()) {
    // if (e instanceof ISourceReference) {
    // final ISourceReference esr = (ISourceReference) e;
    // flags |= ProblemsLabelDecorator
    // .getErrorTicksFromMarkers(e.getResource(),
    // IResource.DEPTH_INFINITE, esr);
    // }
    // }
    // }
    // } catch (final CoreException e) {
    // }
    // }
    // return flags;
    // }

    public static Image getDecoratedImage(final ImageDescriptor baseImage,
            final int adornments, final Point size) {
        return ErlideUIPlugin.getImageDescriptorRegistry().get(
                new ErlangElementImageDescriptor(baseImage, adornments, size));
    }

}
