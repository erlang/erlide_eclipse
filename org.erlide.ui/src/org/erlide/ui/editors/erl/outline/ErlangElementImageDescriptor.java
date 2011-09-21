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

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.resource.CompositeImageDescriptor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.Point;
import org.erlide.ui.ErlideImage;
import org.erlide.ui.internal.ErlideUIPlugin;

/**
 * An ErlangImageDescriptor consists of a base image and several adornments. The
 * adornments are computed according to the flags either passed during creation
 * or set via the method <code>setAdornments</code>.
 * 
 * <p>
 * This class may be instantiated; it is not intended to be subclassed.
 * </p>
 * 
 * @since 2.0
 */
public class ErlangElementImageDescriptor extends CompositeImageDescriptor {

    /** Flag to render the 'exported' adornment. */
    public static final int EXPORTED = 0x001;

    public static final int ERROR = 0x100;

    public static final int WARNING = 0x200;

    private final ImageDescriptor fBaseImage;

    private int fFlags;

    private Point fSize;

    /**
     * Creates a new ErlangElementImageDescriptor.
     * 
     * @param baseImage
     *            an image descriptor used as the base image
     * @param flags
     *            flags indicating which adornments are to be rendered. See
     *            <code>setAdornments</code> for valid values.
     * @param size
     *            the size of the resulting image
     * @see #setAdornments(int)
     */
    public ErlangElementImageDescriptor(final ImageDescriptor baseImage,
            final int flags, final Point size) {
        fBaseImage = baseImage;
        Assert.isNotNull(fBaseImage);
        fFlags = flags;
        Assert.isTrue(fFlags >= 0);
        fSize = size;
        Assert.isNotNull(fSize);
    }

    /**
     * Sets the descriptors adornments. Valid values are: <code>EXPORTED</code>,
     * or any combination of those.
     * 
     * @param adornments
     *            the image descriptors adornments
     */
    public void setAdornments(final int adornments) {
        Assert.isTrue(adornments >= 0);
        fFlags = adornments;
    }

    /**
     * Returns the current adornments.
     * 
     * @return the current adornments
     */
    public int getAdornments() {
        return fFlags;
    }

    /**
     * Sets the size of the image created by calling <code>createImage()</code>.
     * 
     * @param size
     *            the size of the image returned from calling
     *            <code>createImage()</code>
     * @see ImageDescriptor#createImage()
     */
    public void setImageSize(final Point size) {
        Assert.isNotNull(size);
        Assert.isTrue(size.x >= 0 && size.y >= 0);
        fSize = size;
    }

    /**
     * Returns the size of the image created by calling
     * <code>createImage()</code>.
     * 
     * @return the size of the image created by calling
     *         <code>createImage()</code>
     * @see ImageDescriptor#createImage()
     */
    public Point getImageSize() {
        return new Point(fSize.x, fSize.y);
    }

    @Override
    protected Point getSize() {
        return fSize;
    }

    @Override
    public boolean equals(final Object object) {
        if (object == null || !this.getClass().equals(object.getClass())) {
            return false;
        }

        final ErlangElementImageDescriptor other = (ErlangElementImageDescriptor) object;
        return fBaseImage.equals(other.fBaseImage) && fFlags == other.fFlags
                && fSize.equals(other.fSize);
    }

    @Override
    public int hashCode() {
        return fBaseImage.hashCode() | fFlags | fSize.hashCode();
    }

    @Override
    protected void drawCompositeImage(final int width, final int height) {
        final ImageData bg = getImageData(fBaseImage);

        drawImage(bg, 0, 0);

        drawTopRight();
        drawBottomRight();
        drawBottomLeft();
    }

    private ImageData getImageData(final ImageDescriptor descriptor) {
        ImageData data = descriptor.getImageData();
        if (data == null) {
            data = DEFAULT_IMAGE_DATA;
            ErlideUIPlugin
                    .logErrorMessage("Image data not available: " + descriptor.toString()); //$NON-NLS-1$
        }
        return data;
    }

    private void drawTopRight() {
    }

    private void drawBottomRight() {
    }

    private void drawBottomLeft() {
        final Point size = getSize();
        int x = 0;
        if ((fFlags & ERROR) != 0) {
            final ImageData data = getImageData(ErlideImage.OVR_ERROR
                    .getDescriptor());
            drawImage(data, x, size.y - data.height);
            x += data.width;
        }
        if ((fFlags & WARNING) != 0) {
            final ImageData data = getImageData(ErlideImage.OVR_WARNING
                    .getDescriptor());
            drawImage(data, x, size.y - data.height);
            x += data.width;
        }

    }
}
