/*******************************************************************************
 * Copyright (c) 2000, 2003 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.wrangler.refactoring.codeinspection.view;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

/**
 * This ImageView class shows how to use SWTImageCanvas to manipulate images.
 * <p>
 * To facilitate the usage, you should setFocus to the canvas at the beginning,
 * and call the dispose at the end.
 * <p>
 * 
 * @author Chengdong Li: cli4@uky.edu
 * @see uky.article.imageviewer.SWTImageCanvas
 * @author György Orosz
 */

public class GraphImageView extends ViewPart {
    // IPath fragmentPath;
    // {
    // Bundle[] bs = Platform
    // .getFragments(Platform
    // .getBundle(org.erlide.wrangler.refactoring.Activator.PLUGIN_ID));
    // if (bs.length < 1) {
    // ErlLogger.debug("Fragment is not loaded?! No C binary is run.");
    //
    // }
    // Bundle fragment = null;
    // for (int i = 0; i < bs.length; ++i) {
    // if (bs[i].getSymbolicName().equals(
    // "org.erlide.wrangler.refactoring.codeinspection")) {
    // fragment = bs[i];
    // break;
    // }
    // }
    //
    // java.net.URL url = FileLocator.find(fragment, new Path(""), null);
    // try {
    // url = FileLocator.resolve(url);
    // } catch (IOException e) {
    // e.printStackTrace();
    // }
    // fragmentPath = new Path(url.getPath());
    // }

    /**
     * Save image action class
     * 
     * @author Gyorgy Orosz
     */
    public class SaveImageAction extends Action {
        /**
         * Constructor
         */
        public SaveImageAction() {
            setText("Save image as...");
            setToolTipText("Save image as...");
            setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
                    .getImageDescriptor(ISharedImages.IMG_ETOOL_SAVEAS_EDIT));

        }

        @Override
        public void run() {
            imageCanvas.onFileSave();
        }

    }

    /**
     * Save file action class
     * 
     * @author Gyorgy Orosz
     */
    public class SaveDOTAction extends Action {
        /**
         * Constructor
         */
        public SaveDOTAction() {
            setText("Save .dot file as...");
            setToolTipText("Save .dot file as...");
            setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
                    .getImageDescriptor(ISharedImages.IMG_ETOOL_SAVEAS_EDIT));

        }

        @Override
        public void run() {
            final FileDialog fileChooser = new FileDialog(
                    imageCanvas.getShell(), SWT.SAVE);
            fileChooser.setText("Save .dot file");
            fileChooser.setFilterPath("");
            fileChooser.setFilterExtensions(new String[] { "*.dot" });
            fileChooser.setFilterNames(new String[] { "Graphviz file "
                    + " (dot)" });
            final String filename = fileChooser.open();
            if (filename != null) {
                try {
                    FileUtils.copyFile(dotFile, new File(filename));
                } catch (final IOException e) {
                    MessageDialog.openError(imageCanvas.getShell(),
                            "Saving error", e.getMessage());
                }
            }
        }
    }

    // public class RotateAction extends Action {
    // public RotateAction() {
    // setText("Rotate image");
    // setToolTipText("Rotate image");
    // // setImageDescriptor(new Ima
    //
    // }
    //
    // @Override
    // public void run() {
    // ImageData src = imageCanvas.getImageData();
    // if (src == null)
    // return;
    // PaletteData srcPal = src.palette;
    // PaletteData destPal;
    // ImageData dest;
    // /* construct a new ImageData */
    // if (srcPal.isDirect) {
    // destPal = new PaletteData(srcPal.redMask, srcPal.greenMask,
    // srcPal.blueMask);
    // } else {
    // destPal = new PaletteData(srcPal.getRGBs());
    // }
    // dest = new ImageData(src.height, src.width, src.depth, destPal);
    // /* rotate by rearranging the pixels */
    // for (int i = 0; i < src.width; i++) {
    // for (int j = 0; j < src.height; j++) {
    // int pixel = src.getPixel(i, j);
    // dest.setPixel(j, src.width - 1 - i, pixel);
    // }
    // }
    // imageCanvas.setImageData(dest);
    // }
    //
    // }
    //
    // public class FitWindowAction extends Action {
    // public FitWindowAction() {
    // setText("");
    // setToolTipText("");
    // setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
    // .getImageDescriptor(ISharedImages.IMG_ELCL_REMOVEALL));
    //
    // }
    //
    // @Override
    // public void run() {
    //
    // }
    //
    // }
    //
    // public class OriginalSizeAction extends Action {
    // public OriginalSizeAction() {
    // setText("");
    // setToolTipText("");
    // setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
    // .getImageDescriptor(ISharedImages.IMG_ELCL_REMOVEALL));
    //
    // }
    //
    // @Override
    // public void run() {
    //
    // }
    //
    // }
    //
    // public class ZoomInAction extends Action {
    // public ZoomInAction() {
    // setText("");
    // setToolTipText("");
    // setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
    // .getImageDescriptor(ISharedImages.IMG_ELCL_REMOVEALL));
    //
    // }
    //
    // @Override
    // public void run() {
    //
    // }
    //
    // }
    //
    // public class ZoomOutAction extends Action {
    // public ZoomOutAction() {
    // setText("");
    // setToolTipText("");
    // setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
    // .getImageDescriptor(ISharedImages.IMG_ELCL_REMOVEALL));
    //
    // }
    //
    // @Override
    // public void run() {
    //
    // }
    //
    // }

    /**
     * View id
     */
    public static String VIEW_ID = "org.erlide.wrangler.codeinspection.graphview";
    SWTImageCanvas imageCanvas;
    private File dotFile = null;

    /**
     * The constructor.
     */
    public GraphImageView() {
    }

    /**
     * Create the GUI.
     * 
     * @param frame
     *            The Composite handle of parent
     */
    @Override
    public void createPartControl(final Composite frame) {
        imageCanvas = new SWTImageCanvas(frame);
        final IToolBarManager mgr = getViewSite().getActionBars()
                .getToolBarManager();
        mgr.add(new SaveImageAction());
        mgr.add(new SaveDOTAction());
    }

    /**
     * Called when we must grab focus.
     * 
     * @see org.eclipse.ui.part.ViewPart#setFocus
     */
    @Override
    public void setFocus() {
        imageCanvas.setFocus();
    }

    /**
     * Called when the View is to be disposed
     */
    @Override
    public void dispose() {
        imageCanvas.dispose();
        super.dispose();
    }

    /**
     * Set view title
     * 
     * @param title
     *            title string
     */
    public void setViewTitle(final String title) {
        setPartName(title);
    }

    /**
     * Stores the given file
     * 
     * @param f
     *            dot file
     */
    public void setDotFile(final File f) {
        dotFile = f;
    }

    /**
     * Loads the given image to the canvas
     * 
     * @param imgpath
     *            image path
     */
    public void setImage(final Image img) {
        imageCanvas.setImage(img);
    }
}
