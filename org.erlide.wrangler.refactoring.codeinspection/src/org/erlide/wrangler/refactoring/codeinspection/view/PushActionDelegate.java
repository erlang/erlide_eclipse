/*******************************************************************************
 * Copyright (c) 2004 Chengdong Li : cdli@ccs.uky.edu
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/cpl-v10.html
 *******************************************************************************/
package org.erlide.wrangler.refactoring.codeinspection.view;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;

/**
 * Action delegate for all toolbar push-buttons.
 * <p>
 * 
 * @author Chengdong Li: cli4@uky.edu
 * 
 */
public class PushActionDelegate implements IViewActionDelegate {
    /** pointer to image view */
    public GraphImageView view = null;

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IViewActionDelegate#init(org.eclipse.ui.IViewPart)
     */
    @Override
    public void init(final IViewPart viewPart) {
        if (viewPart instanceof GraphImageView) {
            view = (GraphImageView) viewPart;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
     */
    @Override
    public void run(final IAction action) {
        final String myId = action.getId();
        final SWTImageCanvas imageCanvas = view.imageCanvas;
        if (myId.equals("toolbar.open")) {
            imageCanvas.onFileOpen();
            return;
        }
        if (imageCanvas.getSourceImage() == null) {
            return;
        }
        if (myId.equals("toolbar.zoomin")) {
            imageCanvas.zoomIn();
            return;
        } else if (myId.equals("toolbar.zoomout")) {
            imageCanvas.zoomOut();
            return;
        } else if (myId.equals("toolbar.fit")) {
            imageCanvas.fitCanvas();
            return;
        } else if (myId.equals("toolbar.rotate")) {
            /* rotate image anti-clockwise */
            final ImageData src = imageCanvas.getImageData();
            if (src == null) {
                return;
            }
            final PaletteData srcPal = src.palette;
            PaletteData destPal;
            ImageData dest;
            /* construct a new ImageData */
            if (srcPal.isDirect) {
                destPal = new PaletteData(srcPal.redMask, srcPal.greenMask,
                        srcPal.blueMask);
            } else {
                destPal = new PaletteData(srcPal.getRGBs());
            }
            dest = new ImageData(src.height, src.width, src.depth, destPal);
            /* rotate by rearranging the pixels */
            for (int i = 0; i < src.width; i++) {
                for (int j = 0; j < src.height; j++) {
                    final int pixel = src.getPixel(i, j);
                    dest.setPixel(j, src.width - 1 - i, pixel);
                }
            }
            imageCanvas.setImageData(dest);
            return;
        } else if (myId.equals("toolbar.original")) {
            imageCanvas.showOriginal();
            return;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action
     * .IAction, org.eclipse.jface.viewers.ISelection)
     */
    @Override
    public void selectionChanged(final IAction action,
            final ISelection selection) {
    }

}
