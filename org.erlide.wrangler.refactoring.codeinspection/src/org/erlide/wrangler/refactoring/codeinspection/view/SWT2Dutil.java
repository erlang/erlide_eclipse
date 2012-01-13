/*******************************************************************************
 * Copyright (c) 2004 Chengdong Li : cdli@ccs.uky.edu
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/cpl-v10.html
 *******************************************************************************/
package org.erlide.wrangler.refactoring.codeinspection.view;

import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;

/**
 * Utility for Java2d transform
 * 
 * @author Chengdong Li: cli4@uky.edu
 * 
 */
public class SWT2Dutil {

    /**
     * Given an arbitrary rectangle, get the rectangle with the given transform.
     * The result rectangle is positive width and positive height.
     * 
     * @param af
     *            AffineTransform
     * @param src
     *            source rectangle
     * @return rectangle after transform with positive width and height
     */
    public static Rectangle transformRect(final AffineTransform af,
            Rectangle src) {
        final Rectangle dest = new Rectangle(0, 0, 0, 0);
        src = absRect(src);
        Point p1 = new Point(src.x, src.y);
        p1 = transformPoint(af, p1);
        dest.x = p1.x;
        dest.y = p1.y;
        dest.width = (int) (src.width * af.getScaleX());
        dest.height = (int) (src.height * af.getScaleY());
        return dest;
    }

    /**
     * Given an arbitrary rectangle, get the rectangle with the inverse given
     * transform. The result rectangle is positive width and positive height.
     * 
     * @param af
     *            AffineTransform
     * @param src
     *            source rectangle
     * @return rectangle after transform with positive width and height
     */
    public static Rectangle inverseTransformRect(final AffineTransform af,
            Rectangle src) {
        final Rectangle dest = new Rectangle(0, 0, 0, 0);
        src = absRect(src);
        Point p1 = new Point(src.x, src.y);
        p1 = inverseTransformPoint(af, p1);
        dest.x = p1.x;
        dest.y = p1.y;
        dest.width = (int) (src.width / af.getScaleX());
        dest.height = (int) (src.height / af.getScaleY());
        return dest;
    }

    /**
     * Given an arbitrary point, get the point with the given transform.
     * 
     * @param af
     *            affine transform
     * @param pt
     *            point to be transformed
     * @return point after tranform
     */
    public static Point transformPoint(final AffineTransform af, final Point pt) {
        final Point2D src = new Point2D.Float(pt.x, pt.y);
        final Point2D dest = af.transform(src, null);
        final Point point = new Point((int) Math.floor(dest.getX()),
                (int) Math.floor(dest.getY()));
        return point;
    }

    /**
     * Given an arbitrary point, get the point with the inverse given transform.
     * 
     * @param af
     *            AffineTransform
     * @param pt
     *            source point
     * @return point after transform
     */
    public static Point inverseTransformPoint(final AffineTransform af,
            final Point pt) {
        final Point2D src = new Point2D.Float(pt.x, pt.y);
        try {
            final Point2D dest = af.inverseTransform(src, null);
            return new Point((int) Math.floor(dest.getX()),
                    (int) Math.floor(dest.getY()));
        } catch (final Exception e) {
            e.printStackTrace();
            return new Point(0, 0);
        }
    }

    /**
     * Given arbitrary rectangle, return a rectangle with upper-left start and
     * positive width and height.
     * 
     * @param src
     *            source rectangle
     * @return result rectangle with positive width and height
     */
    public static Rectangle absRect(final Rectangle src) {
        final Rectangle dest = new Rectangle(0, 0, 0, 0);
        if (src.width < 0) {
            dest.x = src.x + src.width + 1;
            dest.width = -src.width;
        } else {
            dest.x = src.x;
            dest.width = src.width;
        }
        if (src.height < 0) {
            dest.y = src.y + src.height + 1;
            dest.height = -src.height;
        } else {
            dest.y = src.y;
            dest.height = src.height;
        }
        return dest;
    }
}
