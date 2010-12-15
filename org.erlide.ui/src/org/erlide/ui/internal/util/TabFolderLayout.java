/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.internal.util;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Layout;

public class TabFolderLayout extends Layout {

    @Override
    protected Point computeSize(final Composite composite, final int wHint,
            final int hHint, final boolean flushCache) {
        if (wHint != SWT.DEFAULT && hHint != SWT.DEFAULT) {
            return new Point(wHint, hHint);
        }

        final Control[] children = composite.getChildren();
        final int count = children.length;
        int maxWidth = 0, maxHeight = 0;
        for (int i = 0; i < count; i++) {
            final Control child = children[i];
            final Point pt = child.computeSize(SWT.DEFAULT, SWT.DEFAULT,
                    flushCache);
            maxWidth = Math.max(maxWidth, pt.x);
            maxHeight = Math.max(maxHeight, pt.y);
        }

        if (wHint != SWT.DEFAULT) {
            maxWidth = wHint;
        }
        if (hHint != SWT.DEFAULT) {
            maxHeight = hHint;
        }

        return new Point(maxWidth, maxHeight);

    }

    @Override
    protected void layout(final Composite composite, final boolean flushCache) {
        final Rectangle rect = composite.getClientArea();

        final Control[] children = composite.getChildren();
        for (final Control element : children) {
            element.setBounds(rect);
        }
    }
}
