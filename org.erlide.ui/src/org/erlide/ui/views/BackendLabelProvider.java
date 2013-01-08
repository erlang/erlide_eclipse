/*******************************************************************************
 * Copyright (c) 2009 * and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     *
 *******************************************************************************/
package org.erlide.ui.views;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.erlide.backend.BackendData;
import org.erlide.backend.IBackend;

public class BackendLabelProvider extends LabelProvider {

    @Override
    public Image getImage(final Object element) {
        return null;
    }

    @Override
    public String getText(final Object element) {
        final IBackend b = (IBackend) element;
        final BackendData data = b.getData();
        final String s = data.getRuntimeName();
        // if (s == null) {
        // return "<default>";
        // }
        return s + ": " + data.getNodeName();
    }

}
