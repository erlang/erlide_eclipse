/*******************************************************************************
 * Copyright (c) 2009-2013 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.views;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.erlide.backend.api.BackendData;
import org.erlide.backend.api.IBackend;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;

public class BackendLabelProvider extends LabelProvider {

    @Override
    public Image getImage(final Object element) {
        return null;
    }

    @Override
    public String getText(final Object element) {
        final IBackend b = (IBackend) element;
        final BackendData data = b.getData();
        final RuntimeInfo info = data.getRuntimeInfo();
        final String s = info != null ? info.getName() : "<none>";
        return s + ": " + data.getNodeName();
    }

}
