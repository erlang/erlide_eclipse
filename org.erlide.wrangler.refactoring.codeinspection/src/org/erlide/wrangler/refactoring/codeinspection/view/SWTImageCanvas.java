/*******************************************************************************
 * Copyright (c) 2004 Chengdong Li : cdli@ccs.uky.edu
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/cpl-v10.html
 *******************************************************************************/
package org.erlide.wrangler.refactoring.codeinspection.view;

import java.awt.geom.AffineTransform;

import org.eclipse.core.runtime.Path;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.ImageLoader;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.ScrollBar;

/**
 * A scrollable image canvas that extends org.eclipse.swt.graphics.Canvas.
 * <p/>
 * It requires Eclipse (version >= 2.1) on Win32/win32; Linux/gtk;
 * MacOSX/carbon.
 * <p/>
 * This implementation using the pure SWT, no UI AWT package is used. For
 * convenience, I put everything into one class. However, the best way to
 * implement this is to use inheritance to create multiple hierarchies.
 * 
 * @author Chengdong Li: cli4@uky.edu
 */
public class SWTImageCanvas extends Canvas {
    /* zooming rates in x and y direction are equal. */
    final float ZOOMIN_RATE = 1.1f; /* zoomin rate */
    final float ZOOMOUT_RATE = 0.9f; /* zoomout rate */
    private Image sourceImage; /* original image */
    Image screenImage; /* screen image */
    private AffineTransform transform = new AffineTransform();

    private String currentDir = ""; /* remembering file open directory */

    public SWTImageCanvas(final Composite parent) {
        this(parent, SWT.NULL);
    }

    /**
     * Constructor for ScrollableCanvas.
     * 
     * @param parent
     *            the parent of this control.
     * @param style
     *            the style of this control.
     */
    public SWTImageCanvas(final Composite parent, final int style) {
        super(parent, style | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL
                | SWT.NO_BACKGROUND);
        addControlListener(new ControlAdapter() { /* resize listener. */
            @Override
            public void controlResized(final ControlEvent event) {
                syncScrollBars();
            }
        });
        addPaintListener(new PaintListener() { /* paint listener. */
            @Override
            public void paintControl(final PaintEvent event) {
                paint(event.gc);
            }
        });
        initScrollBars();
    }

    /**
     * Dispose the garbage here
     */
    @Override
    public void dispose() {
        if (sourceImage != null && !sourceImage.isDisposed()) {
            sourceImage.dispose();
        }
        if (screenImage != null && !screenImage.isDisposed()) {
            screenImage.dispose();
        }
    }

    /* Paint function */
    private void paint(final GC gc) {
        final Rectangle clientRect = getClientArea(); /* Canvas' painting area */
        if (sourceImage != null) {
            Rectangle imageRect = SWT2Dutil.inverseTransformRect(transform,
                    clientRect);
            final int gap = 2; /* find a better start point to render */
            imageRect.x -= gap;
            imageRect.y -= gap;
            imageRect.width += 2 * gap;
            imageRect.height += 2 * gap;

            final Rectangle imageBound = sourceImage.getBounds();
            imageRect = imageRect.intersection(imageBound);
            final Rectangle destRect = SWT2Dutil.transformRect(transform,
                    imageRect);

            if (screenImage != null) {
                screenImage.dispose();
            }
            screenImage = new Image(getDisplay(), clientRect.width,
                    clientRect.height);
            final GC newGC = new GC(screenImage);
            newGC.setClipping(clientRect);
            newGC.drawImage(sourceImage, imageRect.x, imageRect.y,
                    imageRect.width, imageRect.height, destRect.x, destRect.y,
                    destRect.width, destRect.height);
            newGC.dispose();

            gc.drawImage(screenImage, 0, 0);
        } else {
            gc.setClipping(clientRect);
            gc.fillRectangle(clientRect);
            initScrollBars();
        }
    }

    /* Initalize the scrollbar and register listeners. */
    private void initScrollBars() {
        final ScrollBar horizontal = getHorizontalBar();
        horizontal.setEnabled(false);
        horizontal.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent event) {
                scrollHorizontally((ScrollBar) event.widget);
            }
        });
        final ScrollBar vertical = getVerticalBar();
        vertical.setEnabled(false);
        vertical.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent event) {
                scrollVertically((ScrollBar) event.widget);
            }
        });
    }

    /* Scroll horizontally */
    private void scrollHorizontally(final ScrollBar scrollBar) {
        if (sourceImage == null) {
            return;
        }

        final AffineTransform af = transform;
        final double tx = af.getTranslateX();
        final double select = -scrollBar.getSelection();
        af.preConcatenate(AffineTransform.getTranslateInstance(select - tx, 0));
        transform = af;
        syncScrollBars();
    }

    /* Scroll vertically */
    private void scrollVertically(final ScrollBar scrollBar) {
        if (sourceImage == null) {
            return;
        }

        final AffineTransform af = transform;
        final double ty = af.getTranslateY();
        final double select = -scrollBar.getSelection();
        af.preConcatenate(AffineTransform.getTranslateInstance(0, select - ty));
        transform = af;
        syncScrollBars();
    }

    /**
     * Source image getter.
     * 
     * @return sourceImage.
     */
    public Image getSourceImage() {
        return sourceImage;
    }

    /**
     * Synchronize the scrollbar with the image. If the transform is out of
     * range, it will correct it. This function considers only following factors
     * :<b> transform, image size, client area</b>.
     */
    public void syncScrollBars() {
        if (sourceImage == null) {
            redraw();
            return;
        }

        AffineTransform af = transform;
        final double sx = af.getScaleX(), sy = af.getScaleY();
        double tx = af.getTranslateX(), ty = af.getTranslateY();
        if (tx > 0) {
            tx = 0;
        }
        if (ty > 0) {
            ty = 0;
        }

        final ScrollBar horizontal = getHorizontalBar();
        horizontal.setIncrement((getClientArea().width / 100));
        horizontal.setPageIncrement(getClientArea().width);
        final Rectangle imageBound = sourceImage.getBounds();
        final int cw = getClientArea().width, ch = getClientArea().height;
        if (imageBound.width * sx > cw) { /* image is wider than client area */
            horizontal.setMaximum((int) (imageBound.width * sx));
            horizontal.setEnabled(true);
            if ((int) -tx > horizontal.getMaximum() - cw) {
                tx = -horizontal.getMaximum() + cw;
            }
        } else { /* image is narrower than client area */
            horizontal.setEnabled(false);
            tx = (cw - imageBound.width * sx) / 2; // center if too small.
        }
        horizontal.setSelection((int) -tx);
        horizontal.setThumb(getClientArea().width);

        final ScrollBar vertical = getVerticalBar();
        vertical.setIncrement((getClientArea().height / 100));
        vertical.setPageIncrement(getClientArea().height);
        if (imageBound.height * sy > ch) { /* image is higher than client area */
            vertical.setMaximum((int) (imageBound.height * sy));
            vertical.setEnabled(true);
            if ((int) -ty > vertical.getMaximum() - ch) {
                ty = -vertical.getMaximum() + ch;
            }
        } else { /* image is less higher than client area */
            vertical.setEnabled(false);
            ty = (ch - imageBound.height * sy) / 2; // center if too small.
        }
        vertical.setSelection((int) -ty);
        vertical.setThumb(getClientArea().height);

        /* update transform. */
        af = AffineTransform.getScaleInstance(sx, sy);
        af.preConcatenate(AffineTransform.getTranslateInstance(tx, ty));
        transform = af;

        redraw();
    }

    /**
     * Reload image from a file
     * 
     * @param filename
     *            image file
     * @return swt image created from image file
     */
    public Image loadImage(final String filename) {
        if (sourceImage != null && !sourceImage.isDisposed()) {
            sourceImage.dispose();
            sourceImage = null;
        }
        sourceImage = new Image(getDisplay(), filename);
        showOriginal();
        return sourceImage;
    }

    /**
     * Load image
     * 
     * @param img
     *            image object
     */
    public void setImage(final Image img) {
        sourceImage = img;
        showOriginal();
    }

    /**
     * Save image to a file
     */
    public void onFileSave() {
        final FileDialog fileChooser = new FileDialog(getShell(), SWT.SAVE);
        fileChooser.setText("Save image file");
        fileChooser.setFilterPath(currentDir);
        fileChooser.setFilterExtensions(new String[] { "*.jpg;*.png" });
        fileChooser.setFilterNames(new String[] { "Image file "
                + " (jpeg, png)" });
        final String filename = fileChooser.open();
        if (filename != null) {
            final ImageLoader imageLoader = new ImageLoader();
            imageLoader.data = new ImageData[] { sourceImage.getImageData() };
            final Path p = new Path(filename);
            if (p.getFileExtension() == "jpg") {
                imageLoader.save(filename, SWT.IMAGE_JPEG);
            } else {
                imageLoader.save(filename, SWT.IMAGE_PNG);
            }

        }

    }

    /**
     * Call back funtion of button "open". Will open a file dialog, and choose
     * the image file. It supports image formats supported by Eclipse.
     */
    public void onFileOpen() {
        final FileDialog fileChooser = new FileDialog(getShell(), SWT.OPEN);
        fileChooser.setText("Open image file");
        fileChooser.setFilterPath(currentDir);
        fileChooser
                .setFilterExtensions(new String[] { "*.gif; *.jpg; *.png; *.ico; *.bmp" });
        fileChooser.setFilterNames(new String[] { "SWT image"
                + " (gif, jpeg, png, ico, bmp)" });
        final String filename = fileChooser.open();
        if (filename != null) {
            loadImage(filename);
            currentDir = fileChooser.getFilterPath();
        }
    }

    /**
     * Get the image data. (for future use only)
     * 
     * @return image data of canvas
     */
    public ImageData getImageData() {
        return sourceImage.getImageData();
    }

    /**
     * Reset the image data and update the image
     * 
     * @param data
     *            image data to be set
     */
    public void setImageData(final ImageData data) {
        if (sourceImage != null) {
            sourceImage.dispose();
        }
        if (data != null) {
            sourceImage = new Image(getDisplay(), data);
        }
        syncScrollBars();
    }

    /**
     * Fit the image onto the canvas
     */
    public void fitCanvas() {
        if (sourceImage == null) {
            return;
        }
        final Rectangle imageBound = sourceImage.getBounds();
        final Rectangle destRect = getClientArea();
        final double sx = (double) destRect.width / (double) imageBound.width;
        final double sy = (double) destRect.height / (double) imageBound.height;
        final double s = Math.min(sx, sy);
        final double dx = 0.5 * destRect.width;
        final double dy = 0.5 * destRect.height;
        centerZoom(dx, dy, s, new AffineTransform());
    }

    /**
     * Show the image with the original size
     */
    public void showOriginal() {
        if (sourceImage == null) {
            return;
        }
        transform = new AffineTransform();
        syncScrollBars();
    }

    /**
     * Perform a zooming operation centered on the given point (dx, dy) and
     * using the given scale factor. The given AffineTransform instance is
     * preconcatenated.
     * 
     * @param dx
     *            center x
     * @param dy
     *            center y
     * @param scale
     *            zoom rate
     * @param af
     *            original affinetransform
     */
    public void centerZoom(final double dx, final double dy,
            final double scale, final AffineTransform af) {
        af.preConcatenate(AffineTransform.getTranslateInstance(-dx, -dy));
        af.preConcatenate(AffineTransform.getScaleInstance(scale, scale));
        af.preConcatenate(AffineTransform.getTranslateInstance(dx, dy));
        transform = af;
        syncScrollBars();
    }

    /**
     * Zoom in around the center of client Area.
     */
    public void zoomIn() {
        if (sourceImage == null) {
            return;
        }
        final Rectangle rect = getClientArea();
        final int w = rect.width, h = rect.height;
        final double dx = (double) w / 2;
        final double dy = (double) h / 2;
        centerZoom(dx, dy, ZOOMIN_RATE, transform);
    }

    /**
     * Zoom out around the center of client Area.
     */
    public void zoomOut() {
        if (sourceImage == null) {
            return;
        }
        final Rectangle rect = getClientArea();
        final int w = rect.width, h = rect.height;
        final double dx = (double) w / 2;
        final double dy = (double) h / 2;
        centerZoom(dx, dy, ZOOMOUT_RATE, transform);
    }
}
