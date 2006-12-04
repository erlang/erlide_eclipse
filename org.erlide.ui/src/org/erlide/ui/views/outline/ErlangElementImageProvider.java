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
package org.erlide.ui.views.outline;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.erlide.basicui.util.ImageDescriptorRegistry;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.ErlideUIPluginImages;

/**
 * Default strategy of the Java plugin for the construction of Java element
 * icons.
 */
/* FIXME: Add usage or remove DESC_OBJ_PROJECT_* */
/* FIXME: Add usage or remove useLightIcons */
@SuppressWarnings("unused")
public class ErlangElementImageProvider {

	/**
	 * Flags for the JavaImageLabelProvider: Generate images with overlays.
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
	 *            Flags as defined by the JavaImageLabelProvider
	 */
	public Image getImageLabel(Object element, int flags) {
		return getImageLabel(computeDescriptor(element, flags));
	}

	private Image getImageLabel(ImageDescriptor descriptor) {
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

	private ImageDescriptor computeDescriptor(Object element, int flags) {
		if (element instanceof IErlElement) {
			return getErlImageDescriptor((IErlElement) element, flags);
		} else if (element instanceof IFile) {
			final IFile file = (IFile) element;
			if ("erl".equals(file.getFileExtension())) { //$NON-NLS-1$
				return getCUResourceImageDescriptor(file, flags); // image for
				// a CU not
				// on the build path
			}
			return getWorkbenchImageDescriptor(file, flags);
		} else if (element instanceof IAdaptable) {
			return getWorkbenchImageDescriptor((IAdaptable) element, flags);
		}
		return null;
	}

	private static boolean showOverlayIcons(int flags) {
		return (flags & OVERLAY_ICONS) != 0;
	}

	private static boolean useSmallSize(int flags) {
		return (flags & SMALL_ICONS) != 0;
	}

	private static boolean useLightIcons(int flags) {
		return (flags & LIGHT_TYPE_ICONS) != 0;
	}

	/**
	 * Returns an image descriptor for a module not on the class path. The
	 * descriptor includes overlays, if specified.
	 */
	public ImageDescriptor getCUResourceImageDescriptor(IFile file, int flags) {
		final Point size = useSmallSize(flags) ? SMALL_SIZE : BIG_SIZE;
		return new ErlangElementImageDescriptor(
				ErlideUIPluginImages.DESC_MODULE_RESOURCE, 0, size);
	}

	/**
	 * Returns an image descriptor for a java element. The descriptor includes
	 * overlays, if specified.
	 */
	public ImageDescriptor getErlImageDescriptor(IErlElement element, int flags) {
		final int adornmentFlags = computeErlangAdornmentFlags(element, flags);
		final Point size = useSmallSize(flags) ? SMALL_SIZE : BIG_SIZE;
		return new ErlangElementImageDescriptor(getBaseImageDescriptor(element,
				flags), adornmentFlags, size);
	}

	/**
	 * Returns an image descriptor for a IAdaptable. The descriptor includes
	 * overlays, if specified (only error ticks apply). Returns
	 * <code>null</code> if no image could be found.
	 */
	public ImageDescriptor getWorkbenchImageDescriptor(IAdaptable adaptable,
			int flags) {
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
	 * Returns an image descriptor for a java element. This is the base image,
	 * no overlays.
	 */
	public ImageDescriptor getBaseImageDescriptor(IErlElement element,
			int renderFlags) {

		// try {
		if (element.getElementType() == IErlElement.FUNCTION) {
			final IErlFunction fun = (IErlFunction) element;
			// int flags= method.getFlags();
			if (fun.isExported()) {
				return ErlideUIPluginImages.DESC_FUNCTION_EXPORTED;
			} else {
				return ErlideUIPluginImages.DESC_FUNCTION_DEFAULT;
			}
		} else if (element.getElementType() == IErlElement.ATTRIBUTE) {
			return ErlideUIPluginImages.DESC_ATTRIBUTE;
		} else if (element.getElementType() == IErlElement.CLAUSE) {
			return ErlideUIPluginImages.DESC_FUNCTION_CLAUSE;
		} else if (element.getElementType() == IErlElement.EXPORT) {
			return ErlideUIPluginImages.DESC_EXPORT;
		} else if (element.getElementType() == IErlElement.RECORD_DEF) {
			return ErlideUIPluginImages.DESC_RECORD_DEF;
		} else if (element.getElementType() == IErlElement.MACRO_DEF) {
			return ErlideUIPluginImages.DESC_MACRO_DEF;
		} else if (element.getElementType() == IErlElement.IMPORT) {
			return ErlideUIPluginImages.DESC_IMPORT;
		} else if (element.getElementType() == IErlElement.ERROR) {
			return ErlideUIPluginImages.DESC_UNKNOWN;
		}

		// Assert.isTrue(false, "ErlangElementImageProvider: wrong image");
		// return ErlideUIPluginImages.DESC_GHOST;
		return ErlideUIPluginImages.DESC_UNKNOWN;

		// } catch (ErlModelException e) {
		// if (e.isDoesNotExist())
		// return ErlideUIPluginImages.DESC_OBJS_UNKNOWN;
		// ErlideUIPlugin.log(e);
		// return ErlideUIPluginImages.DESC_OBJS_GHOST;
		// }
	}

	public void dispose() {
	}

	// ---- Methods to compute the adornments flags
	// ---------------------------------

	private int computeErlangAdornmentFlags(IErlElement element, int renderFlags) {
		int flags = 0;
		if (showOverlayIcons(renderFlags) && element instanceof IErlFunction) {
			try {
				final IErlFunction member = (IErlFunction) element;

				if (member.isExported()) {
					flags |= ErlangElementImageDescriptor.EXPORTED;
				}

			} catch (final Exception e) {
				// do nothing. Can't compute runnable adornment or get flags
			}
		}
		return flags;
	}

	public static Image getDecoratedImage(ImageDescriptor baseImage,
			int adornments, Point size) {
		return ErlideUIPlugin.getImageDescriptorRegistry().get(
				new ErlangElementImageDescriptor(baseImage, adornments, size));
	}

}
